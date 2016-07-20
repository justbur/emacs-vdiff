;;; vdiff.el --- A diff tool similar to  vimdiff -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/justbur/emacs-vdiff
;; Version: 0
;; Keywords:
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A tool like vimdiff for Emacs

;; * Introduction

;; vdiff is a diff tool for Emacs that is made to behave like vimdiff, meaning
;; diff information is displayed in buffers as you edit them. There are commands
;; for cycling through the hunks detected by =diff= and applying changes from
;; one buffer to the other. The main features are

;;   1. Synchronized scrolling of the buffers with lines matching between the
;;      two
;;   2. Commands to transmit (send/receive) changes between buffers
;;   3. Automatic folding of lines that are unchanged in both buffers
;;   4. Commands to jump easily between hunks
;;   5. Everything done through overlays, meaning vdiff doesn't alter the actual
;;      text in the buffer (unless you are transmit changes of course)
;;   6. Unlike ediff, remain in buffers instead of having to use a third "control
;;      buffer"
;;   7. Cool hydra (see below)

;; Contributions and suggestions are very welcome.

;; See https://github.com/justbur/emacs-vdiff for more information

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup vdiff nil
  "Diff tool that is like vimdiff"
  :tag "Vdiff"
  :group 'tools)

(defcustom vdiff-lock-scrolling t
  "Whether to lock scrolling by default when starting
`vdiff-mode'."
  :group 'vdiff
  :type 'boolean)

(defcustom vdiff-diff-program "diff"
  "diff program to use."
  :group 'vdiff
  :type 'string)

(defcustom vdiff-diff-program-args ""
  "Extra arguments to pass to diff. If this is set wrong, you may
break vdiff. It is empty by default."
  :group 'vdiff
  :type 'string)

(defcustom vdiff-fold-padding 6
  "Unchanged lines to leave unfolded around a fold"
  :group 'vdiff
  :type 'integer)

(defcustom vdiff-min-fold-size 4
  "Minimum number of lines to fold"
  :group 'vdiff
  :type 'integer)

(defcustom vdiff-fold-string-function 'vdiff-fold-string-default
  "Function that returns the string printed for a closed
fold. The arguments passed are the number of lines folded, the
text on the first line, and the width of the buffer."
  :group 'vdiff
  :type 'function)

(defcustom vdiff-default-refinement-syntax-code "w"
  "Default syntax table class code to use for identifying
\"words\" in \`vdiff-refine-this-hunk'. Some useful options are

\"w\"   (default) words
\"w_\"  symbols \(really words plus symbol constituents\)

For more information see
https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html"
  :group 'vdiff
  :type 'string)

(defcustom vdiff-subtraction-style 'full
  "How to represent subtractions (i.e., deleted lines). The
default is full which means add the same number of (fake) lines
as those that were removed. The choice single means add only one
fake line. The choice fringe means don't add lines but do
indicate the subtraction location in the fringe."
  :group 'vdiff
  :type '(radio (const :tag "Add same number of fake lines" full)
                (const :tag "Add single line" single)
                (const :tag "Add no lines but use fringe" fringe)))

(defcustom vdiff-subtraction-fill-char ?-
  "Character to use for filling subtraction lines. See also
`vdiff-subtraction-style'."
  :group 'vdiff
  :type 'integer)

(defface vdiff-addition-face
  '((t :inherit diff-added))
  "Face for additions"
  :group 'vdiff)

(defface vdiff-change-face
  '((t :inherit diff-changed))
  "Face for changes"
  :group 'vdiff)

(defface vdiff-closed-fold-face
  '((t :inherit region))
  "Face for closed folds"
  :group 'vdiff)

(defface vdiff-open-fold-face
  '((t))
  "Face for open folds"
  :group 'vdiff)

(defface vdiff-subtraction-face
  '((t :inherit diff-removed))
  "Face for subtractions"
  :group 'vdiff)

(defface vdiff-subtraction-fringe-face
  '((t :inherit vdiff-subtraction-face))
  "Face for subtraction fringe indicators"
  :group 'vdiff)

(defface vdiff-word-changed-face
  '((t :inherit highlight))
  "Face for word changes within a hunk"
  :group 'vdiff)

(defvar vdiff--force-sync-commands '(next-line
                                     previous-line
                                     beginning-of-buffer
                                     end-of-buffer)
  "Commands that trigger sync in other buffer. There should not
be a need to include commands that scroll the buffer here,
because those are handled differently.")

(defvar vdiff--buffers nil)
(defvar vdiff--process-buffer " *vdiff* ")
(defvar vdiff--word-diff-output-buffer " *vdiff-word* ")
(defvar vdiff--diff-data nil)
(defvar vdiff--diff-code-regexp
  "^\\([0-9]+\\),?\\([0-9]+\\)?\\([adc]\\)\\([0-9]+\\),?\\([0-9]+\\)?")
(defvar vdiff--inhibit-window-switch nil)
(defvar vdiff--inhibit-diff-update nil)
(defvar vdiff--in-scroll-hook nil)
;; (defvar vdiff--in-post-command-hook nil)
(defvar vdiff--a-b-line-map nil)
(defvar vdiff--b-a-line-map nil)
(defvar vdiff--folds nil)
(defvar vdiff--all-folds-open nil)
(defvar vdiff--setting-vscroll nil)
(defvar vdiff--diff-stale nil)
(defvar vdiff--after-change-timer nil)
(defvar vdiff--after-change-refresh-delay 1)
(defvar vdiff--window-configuration nil)
(defvar vdiff--new-command nil)

;; * Utilities

(defun vdiff--maybe-int (str)
  (let ((num (and str (string-to-number str))))
    (when (and (numberp num)
               (> num 0))
      num)))

(defun vdiff--buffer-a-p ()
  (eq (current-buffer) (car vdiff--buffers)))

(defun vdiff--buffer-b-p ()
  (eq (current-buffer) (cadr vdiff--buffers)))

(defun vdiff--buffer-p ()
  (memq (current-buffer) vdiff--buffers))

(defun vdiff--other-buffer ()
  (if (vdiff--buffer-a-p)
      (cadr vdiff--buffers)
    (car vdiff--buffers)))

(defun vdiff--other-window ()
  (get-buffer-window (vdiff--other-buffer)))

(defun vdiff--min-window-width ()
  (apply #'min
         (mapcar (lambda (buf)
                   (window-width (get-buffer-window buf)))
                 vdiff--buffers)))

(defun vdiff--move-to-line (n)
  (goto-char (point-min))
  (forward-line (1- n)))

(defun vdiff--overlay-at-pos (&optional pos)
  (let ((pos (or pos (point))))
    (catch 'yes
      (dolist (ovr (overlays-at pos))
        (when (overlay-get ovr 'vdiff-type)
          (throw 'yes ovr))))))

(defun vdiff--hunk-at-point-p ()
  (let ((ovr (vdiff--overlay-at-pos)))
    (and (overlayp ovr)
         (overlay-get ovr 'vdiff-type)
         (not (eq (overlay-get ovr 'vdiff-type) 'fold)))))

(defun vdiff--fold-at-point-p ()
  (let ((ovr (vdiff--overlay-at-pos)))
    (and (overlayp ovr)
         (overlay-get ovr 'vdiff-type)
         (eq (overlay-get ovr 'vdiff-type) 'fold))))

(defun vdiff--overlays-in-region (beg end)
  (let (ovrs)
    (dolist (ovr (overlays-in beg end))
      (when (overlay-get ovr 'vdiff-type)
        (push ovr ovrs)))
    (nreverse ovrs)))

(defun vdiff--maybe-exit-overlay (&optional up no-fold)
  (let* ((ovr (vdiff--overlay-at-pos))
         (type (when ovr (overlay-get ovr 'vdiff-type))))
    (when (and type
               (or (not no-fold)
                   (not (eq type 'fold))))
      (goto-char
       (if up
           (1- (overlay-start ovr))
         (1+ (overlay-end ovr)))))))

(defmacro vdiff--with-other-window (&rest body)
  "Execute BODY in other vdiff window."
  `(when (and (vdiff--buffer-p)
              (not vdiff--inhibit-window-switch)
              (vdiff--other-window))
     (setq vdiff--inhibit-window-switch t)
     (save-selected-window
       (unwind-protect
           (progn
             (select-window (vdiff--other-window))
             ,@body)
         (setq vdiff--inhibit-window-switch nil)))))

(defmacro vdiff--with-all-buffers (&rest body)
  "Execute BODY in all vdiff buffers."
  `(dolist (buf vdiff--buffers)
     (when (buffer-live-p buf)
       (with-current-buffer buf
         ,@body))))

;; * Main overlay refresh routine

(defun vdiff-refresh ()
  "Asynchronously refresh diff information."
  (interactive)
  (let* ((tmp-a (make-temp-file "vdiff-a-"))
         (tmp-b (make-temp-file "vdiff-b-"))
         (cmd (mapconcat #'identity
                         (list
                          vdiff-diff-program
                          vdiff-diff-program-args
                          tmp-a tmp-b)
                         " "))
         (proc (get-buffer-process
                vdiff--process-buffer)))
    (with-current-buffer (car vdiff--buffers)
      (write-region nil nil tmp-a nil 'quietly))
    (with-current-buffer (cadr vdiff--buffers)
      (write-region nil nil tmp-b nil 'quietly))
    (when proc
      (kill-process proc))
    (with-current-buffer (get-buffer-create vdiff--process-buffer)
      (erase-buffer))
    (setq proc (start-process-shell-command
                vdiff--process-buffer
                vdiff--process-buffer
                cmd))
    (process-put proc 'vdiff-tmp-a tmp-a)
    (process-put proc 'vdiff-tmp-b tmp-b)
    (set-process-sentinel proc #'vdiff--diff-refresh-1)))

(defun vdiff--normalize-range (code buf-a beg end)
  (let* ((beg (vdiff--maybe-int beg))
         (end (vdiff--maybe-int end)))
    (cond ((or (and (string= code "a") buf-a)
               (and (string= code "d") (null buf-a)))
           (if end
               (error "vdiff: multi-line range for a or d code")
             (cons (1+ beg) (1+ beg))))
          (t
           (cons beg (or end beg))))))

(defun vdiff--diff-refresh-1 (proc event)
  "This is the sentinel for `vdiff-refresh'. It does the job of
parsing the diff output and triggering the overlay updates."
  (unless vdiff--inhibit-diff-update
    (let (finished)
      (cond ((string= "finished\n" event)
             ;; means no difference between files
             (setq vdiff--diff-data nil)
             (setq finished t))
            ((string= "exited abnormally with code 1\n" event)
             (setq vdiff--diff-data nil)
             (setq finished t)
             (let (res)
               (with-current-buffer (process-buffer proc)
                 (goto-char (point-min))
                 (while (re-search-forward vdiff--diff-code-regexp nil t)
                   (let* ((code (match-string 3))
                          (a-range (vdiff--normalize-range
                                    code t (match-string 1) (match-string 2)))
                          (b-range (vdiff--normalize-range
                                    code nil (match-string 4) (match-string 5))))
                     (push (list code a-range b-range) res))))
               (setq vdiff--diff-data (nreverse res))))
            ((string-match-p "exited abnormally with code" event)
             (setq vdiff--diff-data nil)
             (setq finished t)
             (message "vdiff process error: %s" event)))
      (when finished
        (vdiff--refresh-overlays)
        (vdiff--refresh-line-maps)
        (delete-file (process-get proc 'vdiff-tmp-a))
        (delete-file (process-get proc 'vdiff-tmp-b))))
    (setq vdiff--diff-stale nil)))

(defun vdiff--remove-all-overlays ()
  "Remove all vdiff overlays in both vdiff buffers."
  (vdiff--with-all-buffers
   (remove-overlays (point-min) (point-max) 'vdiff t)))

(defun vdiff-save-buffers ()
  "Save all vdiff buffers."
  (interactive)
  (vdiff--with-all-buffers (save-buffer)))

;; * Word diffs

(defun vdiff--overlay-to-words (&optional ovr syntax-code)
  "Convert OVR to string of \"words\", one per line."
  (let* ((ovr (or ovr (vdiff--overlay-at-pos)))
         (word-syn (or syntax-code
                       vdiff-default-refinement-syntax-code))
         (not-word-syn (concat "^" word-syn))
         last-word-end buf-syntax ovr-text)
    (with-current-buffer (overlay-buffer ovr)
      (setq buf-syntax (syntax-table))
      (setq ovr-text (buffer-substring-no-properties
                      (overlay-start ovr)
                      (overlay-end ovr))))
    (with-temp-buffer
      (set-syntax-table buf-syntax)
      (insert ovr-text)
      (goto-char (point-min))
      (skip-syntax-forward not-word-syn)
      (delete-region (point-min) (point))
      (while (not (eobp))
        (skip-syntax-forward word-syn)
        (insert "\n")
        (setq last-word-end (point))
        (skip-syntax-forward not-word-syn)
        (delete-region last-word-end (point)))
      (buffer-string))))

(defun vdiff--diff-words (this-ovr other-ovr &optional syntax-code)
  "Diff \"words\" between THIS-OVR and OTHER-OVR"
  (when (and (eq (overlay-get this-ovr 'vdiff-type) 'change)
             (overlayp other-ovr))
    (let* ((a-words (vdiff--overlay-to-words this-ovr syntax-code))
           (b-words (vdiff--overlay-to-words other-ovr syntax-code))
           (tmp-file-a (make-temp-file "vdiff-word-a-"))
           (tmp-file-b (make-temp-file "vdiff-word-b-"))
           (out-buffer (get-buffer-create
                        vdiff--word-diff-output-buffer))
           (a-result '())
           (b-result '()))
      (write-region a-words nil tmp-file-a nil 'quietly)
      (write-region b-words nil tmp-file-b nil 'quietly)
      (with-current-buffer out-buffer (erase-buffer))
      (let ((exit-code (call-process
                        vdiff-diff-program nil out-buffer nil tmp-file-a tmp-file-b)))
        (delete-file tmp-file-a)
        (delete-file tmp-file-b)
        (when (= exit-code 1)
          (with-current-buffer out-buffer
            (goto-char (point-min))
            (while (re-search-forward vdiff--diff-code-regexp nil t)
              (let ((a-change (list (string-to-number (match-string 1))))
                    (b-change (list (string-to-number (match-string 4)))))
                (forward-line 1)
                (while (and (not (eobp))
                            (not (looking-at-p vdiff--diff-code-regexp)))
                  (cond ((looking-at-p "^<")
                         (push (buffer-substring-no-properties
                                (+ 2 (point)) (line-end-position))
                               a-change))
                        ((looking-at-p "^>")
                         (push (buffer-substring-no-properties
                                (+ 2 (point)) (line-end-position))
                               b-change)))
                  (forward-line 1))
                (when (cdr a-change)
                  (push (nreverse a-change) a-result))
                (when (cdr b-change)
                  (push (nreverse b-change) b-result))))
            (cons (nreverse a-result) (nreverse b-result))))))))

(defun vdiff-refine-this-hunk (&optional syntax-code ovr)
  "Highlight word differences in current hunk.

This uses `vdiff-default-refinement-syntax-code' for the
definition of a \"word\", unless one is provided using
SYNTAX-CODE."
  (interactive (list vdiff-default-refinement-syntax-code
                     (vdiff--overlay-at-pos)))
  (let* ((ovr (or ovr (vdiff--overlay-at-pos)))
         (other-ovr (when (overlayp ovr)
                      (overlay-get ovr 'vdiff-other-overlay)))
         (word-syn (or syntax-code
                       vdiff-default-refinement-syntax-code))
         (not-word-syn (concat "^" word-syn))
         instructions ovr-ins)
    (when (and ovr
               other-ovr
               (consp (setq instructions
                            (vdiff--diff-words ovr other-ovr))))
      (dolist (curr-ovr (list ovr other-ovr))
        (setq ovr-ins (if (eq curr-ovr ovr)
                          (car instructions)
                        (cdr instructions)))
        (with-current-buffer (overlay-buffer curr-ovr)
          (save-excursion
            (let ((current-word-n 1))
              (goto-char (overlay-start curr-ovr))
              (skip-syntax-forward not-word-syn)
              (dolist (ins ovr-ins)
                (dotimes (_ (- (car ins) current-word-n))
                  (skip-syntax-forward word-syn)
                  (skip-syntax-forward not-word-syn))
                (setq current-word-n (car ins))
                (let* ((words (cdr ins))
                       (word-ovr
                        (make-overlay
                         (point)
                         (progn
                           (dotimes (_ (length words))
                             (skip-syntax-forward not-word-syn)
                             (skip-syntax-forward word-syn))
                           (point)))))
                  (cl-incf current-word-n (length words))
                  (overlay-put word-ovr 'vdiff t)
                  (overlay-put word-ovr 'face 'vdiff-word-changed-face)
                  (overlay-put word-ovr 'vdiff-refinement t)
                  (skip-syntax-forward not-word-syn))))))))))

;; Not working yet
;; (defun vdiff-refine-this-hunk-whitespace (ovr)
;;   "Highlight whitespace differences in current hunk."
;;   (interactive (list (vdiff--overlay-at-pos)))
;;   (vdiff-refine-this-hunk "-" ovr))

(defun vdiff-refine-this-hunk-symbol (ovr)
  "Highlight symbol differences in current hunk."
  (interactive (list (vdiff--overlay-at-pos)))
  (vdiff-refine-this-hunk "w_" ovr))

(defun vdiff-refine-this-hunk-word (ovr)
  "Highlight word differences in current hunk."
  (interactive (list (vdiff--overlay-at-pos)))
  (vdiff-refine-this-hunk "w" ovr))

(defun vdiff-remove-refinements-in-hunk (ovr)
  (interactive (list (vdiff--overlay-at-pos)))
  (dolist (chg-ovr (list ovr
                         (overlay-get ovr 'vdiff-other-overlay)))
    (with-current-buffer (overlay-buffer chg-ovr)
      (dolist (sub-ovr (overlays-in
                        (overlay-start chg-ovr)
                        (overlay-end chg-ovr)))
        (when (overlay-get sub-ovr 'vdiff-refinement)
          (delete-overlay sub-ovr))))))

(defun vdiff-refine-all-hunks (&optional syntax-code)
  "Highlight word differences in all hunks.

This uses `vdiff-default-refinement-syntax-code' for the
definition of a \"word\", unless one is provided using
SYNTAX-CODE.
See `vdiff-default-refinement-syntax-code' to change the definition
of a \"word\"."
  (interactive)
  (dolist (ovr (overlays-in (point-min) (point-max)))
    (vdiff-refine-this-hunk syntax-code ovr)))

;; Not working yet
;; (defun vdiff-refine-all-hunks-whitespace ()
;;   "Highlight whitespace differences in all hunks."
;;   (interactive)
;;   (vdiff-refine-all-hunks "-"))

(defun vdiff-refine-all-hunks-symbol ()
  "Highlight symbol differences in all hunks."
  (interactive)
  (vdiff-refine-all-hunks "w_"))

(defun vdiff-refine-all-hunks-word ()
  "Highlight word differences in all hunks."
  (interactive)
  (vdiff-refine-all-hunks "w"))

;; * Add overlays

(defvar vdiff--insertion-arrow-bits
  (cl-map
   #'vector
   (lambda (line)
     (let ((ex (length line)))
       (cl-reduce
        (lambda (acc el)
          (+ acc (* el (expt 2 (cl-decf ex)))))
        line
        :initial-value 0)))
   '((0 0 1 1 1 1 1 1)
     (0 0 0 1 1 1 1 1)
     (0 0 0 0 1 1 1 1)
     (0 0 0 1 1 1 1 1)
     (0 0 1 1 1 0 1 1)
     (0 1 1 1 0 0 0 1)
     (1 1 1 0 0 0 0 0)
     (1 1 0 0 0 0 0 0)
     (1 0 0 0 1 1 1 1))))

(define-fringe-bitmap
  'vdiff--insertion-arrow vdiff--insertion-arrow-bits nil 8 'top)

(defun vdiff--make-subtraction-string (n-lines)
  (let* ((width (1- (vdiff--min-window-width)))
         (win-height (window-height))
         (max-lines (floor (* 0.7 win-height)))
         (truncate (> n-lines max-lines))
         (trunc-n-lines
          (cond ((eq 'single vdiff-subtraction-style) 1)
                (truncate max-lines)
                (t n-lines)))
         (truncate-prefix-len 2)
         string truncate-message)
    (dotimes (_ trunc-n-lines)
      (push (make-string width vdiff-subtraction-fill-char) string))
    (when truncate
      (setq truncate-message (format " +%d lines " (- n-lines trunc-n-lines)))
      (push (concat (make-string truncate-prefix-len vdiff-subtraction-fill-char)
                    truncate-message
                    (make-string (- width truncate-prefix-len
                                    (length truncate-message))
                                 vdiff-subtraction-fill-char))
            string)
      (setq string (nreverse string)))
    (if (eq vdiff-subtraction-style 'fringe)
        (propertize
         " "
         'display '(left-fringe vdiff--insertion-arrow
                                vdiff-subtraction-fringe-face))
      (propertize
       (concat (mapconcat #'identity string "\n") "\n")
       'face 'vdiff-subtraction-face))))

(defun vdiff--add-subtraction-overlay (n-lines)
  (let* ((ovr (make-overlay (point) (1+ (point)))))
    (overlay-put ovr 'before-string (vdiff--make-subtraction-string n-lines))
    (overlay-put ovr 'vdiff-type 'subtraction)
    (overlay-put ovr 'vdiff t)
    ovr))

(defun vdiff--add-hunk-overlay
    (n-lines &optional addition n-subtraction-lines)
  (let ((beg (point))
        (end (save-excursion
               (forward-line n-lines)
               (point))))
    (let ((ovr (make-overlay beg end))
          (type (if addition 'addition 'change))
          (face (if addition 'vdiff-addition-face 'vdiff-change-face)))
      (overlay-put ovr 'vdiff-type type)
      (overlay-put ovr 'face face)
      (overlay-put ovr 'vdiff t)
      (when (and n-subtraction-lines
                 (> n-subtraction-lines 0))
        (overlay-put ovr 'after-string
                     (vdiff--make-subtraction-string n-subtraction-lines)))
      ovr)))

(defun vdiff-fold-string-default (n-lines first-line-text width)
  "Produces default format line for closed folds. See
`vdiff-fold-string-function'."
  (let ((first-line-text (string-trim-left first-line-text))
        (start (format "+--%d lines: " n-lines))
        (width (1- width)))
    (if (> (+ 1 (length first-line-text) (length start)) width)
        (concat start
                (substring-no-properties
                 first-line-text 0 (- width (length start)))
                "\n")
        (concat start
                first-line-text
                (make-string
                 (- width (length start) (length first-line-text))
                 ?-)
                "\n"))))

(defun vdiff--make-fold (buffer range)
  (with-current-buffer buffer
    (let* ((beg-line (car range))
           (end-line (cdr range))
           (fold-start (vdiff--pos-at-line-beginning beg-line))
           (first-line-text
            (buffer-substring-no-properties
             fold-start (save-excursion
                          (goto-char fold-start)
                          (line-end-position))))
           (fold-end
            (vdiff--pos-at-line-beginning end-line))
           (ovr (make-overlay fold-start fold-end))
           (text
            (propertize (funcall vdiff-fold-string-function
                                 (- end-line beg-line)
                                 first-line-text
                                 (vdiff--min-window-width))
                        'face 'vdiff-closed-fold-face)))
      (overlay-put ovr 'face 'vdiff-open-fold-face)
      (overlay-put ovr 'vdiff-fold-text text)
      (overlay-put ovr 'vdiff-type 'fold)
      (overlay-put ovr 'vdiff t)
      ovr)))

(defun vdiff--narrow-fold-range (range)
  (cons (+ vdiff-fold-padding (car range))
        (1+ (- (cdr range) vdiff-fold-padding))))

(defun vdiff--point-in-fold-p (fold)
  (and (eq (current-buffer) (overlay-buffer fold))
       (>= (point) (overlay-start fold))
       (<= (point) (overlay-end fold))))

(defun vdiff--add-folds (a-buffer b-buffer folds)
  (let (new-folds)
    (dolist (fold folds)
      (let ((a-range (vdiff--narrow-fold-range (car fold)))
            (b-range (vdiff--narrow-fold-range (cdr fold))))
        (cond ((assoc a-range vdiff--folds)
               ;; Restore any overlays on same range
               (let* ((a-fold (cadr (assoc a-range vdiff--folds)))
                      (b-fold (cl-caddr (assoc a-range vdiff--folds)))
                      (a-beg (vdiff--pos-at-line-beginning
                              (car a-range) a-buffer))
                      (a-end (vdiff--pos-at-line-beginning
                              (cdr a-range) a-buffer))
                      (b-beg (vdiff--pos-at-line-beginning
                              (car b-range) b-buffer))
                      (b-end (vdiff--pos-at-line-beginning
                              (cdr b-range) b-buffer)))
                 (move-overlay a-fold a-beg a-end a-buffer)
                 (move-overlay b-fold b-beg b-end b-buffer)
                 (push (list a-range a-fold b-fold) new-folds)))
              ((> (1+ (- (cdr a-range) (car a-range))) vdiff-min-fold-size)
               ;; Ranges include padding
               (let ((a-fold (vdiff--make-fold a-buffer a-range))
                     (b-fold (vdiff--make-fold b-buffer b-range)))
                 (dolist (fold (list a-fold b-fold))
                   (cond ((or (vdiff--point-in-fold-p a-fold)
                              (vdiff--point-in-fold-p b-fold)
                              vdiff--all-folds-open)
                          (vdiff--set-open-fold-props fold))
                         (t
                          (vdiff--set-closed-fold-props fold))))
                 (overlay-put a-fold 'vdiff-other-fold b-fold)
                 (overlay-put b-fold 'vdiff-other-fold a-fold)
                 (push (list a-range a-fold b-fold) new-folds))))))
    (setq vdiff--folds new-folds)))

(defun vdiff--remove-fold-overlays (_)
  (setq vdiff--folds nil))

(defun vdiff--add-diff-overlay (in-a code this-len other-len)
  (cond ((or (and in-a (string= code "d"))
             (and (not in-a) (string= code "a")))
         (vdiff--add-hunk-overlay this-len t))
        ((or (and in-a (string= code "a"))
             (and (not in-a) (string= code "d")))
         (vdiff--add-subtraction-overlay other-len))
        (t
         (vdiff--add-hunk-overlay this-len nil (- other-len this-len)))))

(defun vdiff--refresh-overlays ()
  "Delete and recreate overlays in both buffers."
  (vdiff--remove-all-overlays)
  (let ((a-buffer (car vdiff--buffers))
        (b-buffer (cadr vdiff--buffers))
        (a-line 1)
        (b-line 1)
        (a-last-post 1)
        (b-last-post 1)
        (vdiff--inhibit-diff-update t)
        folds)
    (save-excursion
      (with-current-buffer a-buffer
        (widen)
        (goto-char (point-min)))
      (with-current-buffer b-buffer
        (widen)
        (goto-char (point-min)))
      (dolist (header vdiff--diff-data)
        (let* ((code (nth 0 header))
               (a-range (nth 1 header))
               (b-range (nth 2 header))
               (a-beg (car a-range))
               (a-end (cdr a-range))
               (a-post (if (string= code "a") a-end (1+ a-end)))
               (a-len (1+ (- a-end a-beg)))
               (b-beg (car b-range))
               (b-end (cdr b-range))
               (b-post (if (string= code "d") b-end (1+ b-end)))
               (b-len (1+ (- b-end b-beg))))

          (unless (member code (list "a" "d" "c"))
            (user-error "vdiff: Unexpected code in diff output"))

          (push (cons (cons a-last-post (1- a-beg))
                      (cons b-last-post (1- b-beg)))
                folds)
          (setq a-last-post a-post)
          (setq b-last-post b-post)

          (let (ovr-a ovr-b)
            (with-current-buffer a-buffer
              (forward-line (- a-beg a-line))
              (setq a-line a-beg)
              (setq ovr-a (vdiff--add-diff-overlay t code a-len b-len)))
            (with-current-buffer b-buffer
              (forward-line (- b-beg b-line))
              (setq b-line b-beg)
              (setq ovr-b (vdiff--add-diff-overlay nil code b-len a-len)))
            (overlay-put ovr-a 'vdiff-other-overlay ovr-b)
            (overlay-put ovr-b 'vdiff-other-overlay ovr-a))))
      (push (cons (cons a-last-post
                        (with-current-buffer a-buffer
                          (line-number-at-pos (point-max))))
                  (cons b-last-post
                        (with-current-buffer b-buffer
                          (line-number-at-pos (point-max)))))
            folds))
    (vdiff--add-folds a-buffer b-buffer folds)))

;; * Send/Receive changes

(defun vdiff--region-or-close-overlay ()
  "Return region bounds if active. Otherwise check if there is an
overlay at point and return it if there is. If this fails check a
line above. Always search to the end of the current line as
well. This only returns bounds for `interactive'."
  (if (region-active-p)
      (prog1
        (list (region-beginning) (region-end))
        (deactivate-mark))
    (list (if (or (= (line-number-at-pos) 1)
                  (vdiff--overlay-at-pos
                   (line-beginning-position)))
              (line-beginning-position)
            (save-excursion
              (forward-line -1)
              (line-beginning-position)))
          (save-excursion
            (forward-line 1)
            (point)))))

(defun vdiff-send-changes (beg end &optional receive)
  "Send changes in this hunk to other vdiff buffer. If the region
is active, send all changes found in the region. Otherwise use
the hunk under point or on the immediately preceding line."
  (interactive
   (vdiff--region-or-close-overlay))
  (let* ((ovrs (overlays-in beg end))
         (vdiff--inhibit-diff-update t))
    (dolist (ovr ovrs)
      (cond ((and (overlay-get ovr 'vdiff-other-overlay)
                  receive)
             (let* ((other-ovr (overlay-get ovr 'vdiff-other-overlay))
                    (pos (overlay-start other-ovr)))
               (vdiff--with-other-window
                (vdiff-send-changes pos (1+ pos)))))
            ((memq (overlay-get ovr 'vdiff-type)
                   '(change addition))
             (vdiff--transmit-change ovr))
            ((eq (overlay-get ovr 'vdiff-type) 'subtraction)
             (vdiff--transmit-subtraction ovr))))
    (vdiff-refresh)
    (vdiff--scroll-function)))

(defun vdiff-receive-changes (beg end)
  "Receive the changes corresponding to this position from the
other vdiff buffer. If the region is active, receive all
corresponding changes found in the region. Otherwise use the
changes under point or on the immediately preceding line."
  (interactive (vdiff--region-or-close-overlay))
  (vdiff-send-changes beg end t))

(defun vdiff--transmit-change (ovr)
  "Send text in OVR to corresponding overlay in other buffer."
  (if (not (overlayp ovr))
         (message "No change found")
    (let* ((addition (eq 'addition (overlay-get ovr 'vdiff-type)))
           (other-ovr (overlay-get ovr 'vdiff-other-overlay))
           (text (buffer-substring-no-properties
                  (overlay-start ovr)
                  (overlay-end ovr))))
      (with-current-buffer (vdiff--other-buffer)
        (save-excursion
          (goto-char (overlay-start other-ovr))
          (unless addition
            (delete-region (overlay-start other-ovr)
                           (overlay-end other-ovr)))
          (insert text))
        (delete-overlay other-ovr))
      (delete-overlay ovr))))

(defun vdiff--transmit-subtraction (ovr)
  "Same idea as `vdiff--transmit-change' except we are
just deleting text in the other buffer."
  (if (not (overlayp ovr))
         (message "No change found")
    (let* ((other-ovr (overlay-get ovr 'vdiff-other-overlay)))
      (when other-ovr
        (with-current-buffer (vdiff--other-buffer)
          (delete-region (overlay-start other-ovr)
                         (overlay-end other-ovr))
          (delete-overlay other-ovr))))))

;; * Scrolling and line syncing

(defmacro vdiff--calculate-unbalanced-section
    (l-s-map s-l-map l-prior s-prior l-post s-post)
  ;; l-prior 0     0 s-prior
  ;; l-beg   1 +   -
  ;; l-end   2 +   -
  ;; l-post  3     1 s-post
  `(let* ((l-beg (1+ ,l-prior))
          (l-len (1- (- ,l-post ,l-prior))))
     (push (list ,s-prior ,l-prior 0 entry) ,s-l-map)
     (push (list ,s-post  ,l-post 0 entry)  ,s-l-map)
     (push (list ,l-prior ,s-prior 0 entry) ,l-s-map)
     (dotimes (offset (1+ l-len))
       (push (list (+ offset l-beg) ,s-post offset entry)
             ,l-s-map))
     (push (list (1+ ,l-post) (1+ ,s-post) 0 entry) ,l-s-map)))

(defun vdiff--refresh-line-maps ()
  "Sync information in `vdiff--line-map' with
`vdiff--diff-data'."
  (let ((vdiff--inhibit-diff-update t)
        a-b-map b-a-map)
    (dolist (entry vdiff--diff-data)
      (let* ((code (car entry))
             (a-lines (nth 1 entry))
             (a-beg (car a-lines))
             (a-prior (1- a-beg))
             (a-end (cdr a-lines))
             (a-post (if (string= code "a") a-end (1+ a-end)))
             (a-len (1+ (- a-end a-beg)))
             (b-lines (nth 2 entry))
             (b-beg (car b-lines))
             (b-prior (1- b-beg))
             (b-end (cdr b-lines))
             (b-post (if (string= code "d") b-end (1+ b-end)))
             (b-len (1+ (- b-end b-beg))))
        ;; Format is (line-key line-a-to-align line-b-to-align extra-scroll entry-info)
        (cond ((string= code "d")
               ;; a-prior 0     0 b-prior
               ;; a-beg   1 +   -
               ;; a-end   2 +   -
               ;; a-post  3     1 b-beg=b-end=b-post
               (vdiff--calculate-unbalanced-section
                a-b-map b-a-map a-prior b-prior a-post b-post))
              ((string= code "a")
               ;; 0     0
               ;; -     1 +
               ;; -     2 +
               ;; 1     3
               (vdiff--calculate-unbalanced-section
                b-a-map a-b-map b-prior a-prior b-post a-post))
              ((> a-len b-len)
               ;; 0     0   b-prior
               ;; 1 ~   1 ~ b-beg
               ;; 2 ~   2 ~ b-end
               ;; 3 ~   -
               ;; 4 ~   -
               ;; 5     3   b-post
               (push (list a-prior b-prior 0 entry) a-b-map)
               (vdiff--calculate-unbalanced-section
                a-b-map b-a-map (+ a-prior b-len) b-end a-post b-post))
              ((< a-len b-len)
               (push (list b-prior a-prior 0 entry) b-a-map)
               (vdiff--calculate-unbalanced-section
                b-a-map a-b-map (+ b-prior a-len) a-end b-post a-post))
              ((= a-len b-len)
               (push (list a-prior b-prior 0 entry) a-b-map)
               (push (list a-post  b-post 0 entry)  a-b-map)
               (push (list b-prior a-prior 0 entry) b-a-map)
               (push (list b-post  a-post 0 entry)  b-a-map)))))
    (setq vdiff--a-b-line-map (cons (list 0 0 0) (nreverse a-b-map)))
    (setq vdiff--b-a-line-map (cons (list 0 0 0) (nreverse b-a-map)))))

(defun vdiff--translate-line (line &optional B-to-A)
  "Translate LINE in buffer A to corresponding line in buffer
B. Go from buffer B to A if B-to-A is non nil."
  (interactive (list (line-number-at-pos) (vdiff--buffer-b-p)))
  (let ((map (if B-to-A vdiff--b-a-line-map vdiff--a-b-line-map))
        last-entry res)
    (when map
      (setq last-entry
            (catch 'closest
              (let (prev-entry)
                (dolist (entry map)
                  (let ((map-line (car entry)))
                    (cond ((< map-line line)
                           (setq prev-entry entry))
                          ((= map-line line)
                           (throw 'closest entry))
                          (t
                           (throw 'closest prev-entry)))))
                (throw 'closest prev-entry))))
      (unless last-entry
        (setq last-entry (list line line))
        (message "Error in line translation"))
      (prog1
          (setq res (cons (+ (- line (car last-entry)) (cadr last-entry))
                          (nth 2 last-entry)))
        (when (called-interactively-p 'interactive)
          (message "This line: %s; Other line %s; vscroll-state %s; entry %s"
                   line res (cdr res) last-entry))))))

(defun vdiff-switch-buffer (line in-b)
  "Jump to the line in the other vdiff buffer that corresponds to
the current one."
  (interactive (list (line-number-at-pos) (vdiff--buffer-b-p)))
  (vdiff-refresh)
  (select-window (vdiff--other-window))
  (let ((line (car-safe (vdiff--translate-line line in-b))))
    (when line
      (vdiff--move-to-line line))))

(defun vdiff--recenter-both ()
  (recenter)
  (vdiff--with-other-window (recenter)))

(defun vdiff--sync-line (line in-a)
  "Sync point in the other vdiff buffer to the line in this
buffer. This is usually not necessary."
  (interactive (list (line-number-at-pos)
                     (not (vdiff--buffer-a-p))))
  (let ((new-line (car-safe (vdiff--translate-line
                        line (not in-a)))))
    (when new-line
      (vdiff--with-other-window
       (goto-char (vdiff--pos-at-line-beginning new-line))))))

(defun vdiff-sync-and-center ()
  "Sync point in the other vdiff buffer to the line in this
buffer and center both buffers at this line."
  (interactive)
  (vdiff--sync-line (line-number-at-pos) (vdiff--buffer-a-p))
  (vdiff--recenter-both))

(defun vdiff-restore-windows ()
  "Restore initial window configuration."
  (interactive)
  (set-window-configuration vdiff--window-configuration))

(defun vdiff--pos-at-line-beginning (line &optional buffer)
  "Return position at beginning of LINE in BUFFER (or current
buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (vdiff--move-to-line line)
      (line-beginning-position))))

(defun vdiff--set-vscroll-and-force-update (window &optional vscroll)
  (run-at-time
   nil nil
   (lambda ()
     (unless vdiff--setting-vscroll
       (let ((vdiff--setting-vscroll t))
         (when vscroll
           (set-window-vscroll window vscroll))
         (force-window-update window))))))

(defun vdiff--flag-new-command ()
  (setq vdiff--new-command t))

(defun vdiff--scroll-function (&optional window window-start)
  "Sync scrolling of all vdiff windows."
  (let* ((window (or window (selected-window)))
         (window-start (or window-start (window-start)))
         (buf-a (car vdiff--buffers))
         (buf-b (cadr vdiff--buffers))
         (win-a (get-buffer-window buf-a))
         (win-b (get-buffer-window buf-b)))
    (when (and (eq window (selected-window))
               (window-live-p win-a)
               (window-live-p win-b)
               (memq window (list win-a win-b))
               (not vdiff--in-scroll-hook)
               vdiff--new-command)
      (setq vdiff--new-command nil)
      (let* ((in-b (eq window win-b))
             (other-window (if in-b win-a win-b))
             (other-buffer (if in-b buf-a buf-b))
             (this-start-line (line-number-at-pos window-start))
             (start-translation
              (vdiff--translate-line this-start-line in-b))
             (other-curr-start (window-start other-window))
             (other-start-line (car-safe start-translation))
             (other-start-pos (when other-start-line
                                (vdiff--pos-at-line-beginning
                                 other-start-line other-buffer)))
             (scroll-amt (cdr-safe start-translation))
             (this-line (+ (count-lines window-start (point))
                           this-start-line))
             (translation (vdiff--translate-line this-line in-b))
             (other-pos (when translation
                          (vdiff--pos-at-line-beginning
                           (car translation) other-buffer)))
             (vdiff--in-scroll-hook t))
        (when (and other-start-pos
                   other-pos)
          (set-window-point other-window other-pos)
          (unless (= other-curr-start other-start-pos)
            (set-window-start other-window other-start-pos))
          (vdiff--set-vscroll-and-force-update
           other-window
           (when (eq vdiff-subtraction-style 'full)
             scroll-amt)))))))

;; (defun vdiff--post-command-hook ()
;;   "Sync scroll for `vdiff--force-sync-commands'."
;;   ;; Use real-this-command because evil-next-line and evil-previous-line pretend
;;   ;; they are next-line and previous-line
;;   (when (and (memq this-command vdiff--force-sync-commands)
;;              (not vdiff--in-post-command-hook)
;;              (vdiff--buffer-p))
;;     (let ((vdiff--in-post-command-hook t))
;;       (when (and (sit-for 0.05)
;;                  (eq vdiff-subtraction-style 'full))
;;         (vdiff--scroll-function)))))

(defun vdiff--after-change-function (&rest _)
  (unless vdiff--diff-stale
    (setq vdiff--diff-stale t)
    (when (timerp vdiff--after-change-timer)
      (cancel-timer vdiff--after-change-timer))
    (setq vdiff--after-change-timer
          (run-with-idle-timer
           vdiff--after-change-refresh-delay
           nil #'vdiff-refresh))))

(defvar vdiff--bottom-left-angle-bits
  (let ((vec (make-vector 13 (+ (expt 2 7) (expt 2 6)))))
    (aset vec 11 (1- (expt 2 8)))
    (aset vec 12 (1- (expt 2 8)))
    vec))

(define-fringe-bitmap 'vdiff--bottom-left-angle vdiff--bottom-left-angle-bits)

(defvar vdiff--top-left-angle-bits
  (let ((vec (make-vector 13 (+ (expt 2 7) (expt 2 6)))))
    (aset vec 0 (1- (expt 2 8)))
    (aset vec 1 (1- (expt 2 8)))
    vec))

(define-fringe-bitmap 'vdiff--top-left-angle vdiff--top-left-angle-bits)

(defun vdiff--set-open-fold-props (ovr)
  "Set overlay properties to open fold OVR."
  (overlay-put ovr 'vdiff-fold-open t)
  (overlay-put ovr 'display nil)
  (overlay-put ovr 'intangible nil)
  (overlay-put ovr 'before-string
               (propertize
                " " 'display '(left-fringe vdiff--top-left-angle)))
  (overlay-put ovr 'line-prefix
               (propertize
                " " 'display '(left-fringe vertical-bar)))
  (overlay-put ovr 'after-string
               (propertize
                " " 'display '(left-fringe vdiff--bottom-left-angle))))

(defun vdiff--set-closed-fold-props (ovr)
  "Set overlay properties to close fold OVR."
  (overlay-put ovr 'vdiff-fold-open nil)
  (overlay-put ovr 'before-string nil)
  (overlay-put ovr 'line-prefix nil)
  (overlay-put ovr 'after-string nil)
  (overlay-put ovr 'intangible t)
  (overlay-put ovr 'display (overlay-get ovr 'vdiff-fold-text)))

(defun vdiff-open-fold (beg end)
  "Open folds between BEG and END, as well as corresponding ones
in other vdiff buffer. If called interactively, either open fold
at point or on prior line. If the region is active open all folds
in the region."
  (interactive (vdiff--region-or-close-overlay))
  (dolist (ovr (overlays-in beg end))
    (when (eq (overlay-get ovr 'vdiff-type) 'fold)
      (let ((other-fold (overlay-get ovr 'vdiff-other-fold)))
        (vdiff--set-open-fold-props ovr)
        (vdiff--set-open-fold-props other-fold)))))

(defun vdiff-close-fold (beg end)
  "Close folds between BEG and END, as well as corresponding ones
in other vdiff buffer. If called interactively, either close fold
at point or on prior line. If the region is active close all
folds in the region."
  (interactive (vdiff--region-or-close-overlay))
  (dolist (ovr (overlays-in beg end))
    (when (eq (overlay-get ovr 'vdiff-type) 'fold)
      (let ((other-fold (overlay-get ovr 'vdiff-other-fold)))
        (setq vdiff--all-folds-open nil)
        (goto-char (overlay-start ovr))
        (vdiff--set-closed-fold-props ovr)
        (vdiff--set-closed-fold-props other-fold)))))

(defun vdiff-open-all-folds ()
  "Open all folds in both buffers"
  (interactive)
  (setq vdiff--all-folds-open t)
  (vdiff-open-fold (point-min) (point-max)))

(defun vdiff-close-all-folds ()
  "Close all folds in both buffers"
  (interactive)
  (setq vdiff--all-folds-open nil)
  (vdiff-close-fold (point-min) (point-max)))

(defun vdiff-close-other-folds ()
  "Close all other folds in both buffers"
  (interactive)
  (dolist (ovr (overlays-in (point-min) (point-max)))
    (when (and (eq (overlay-get ovr 'vdiff-type) 'fold)
               (not (vdiff--point-in-fold-p ovr)))
      (setq vdiff--all-folds-open nil)
      (vdiff--set-closed-fold-props ovr)
      (vdiff--set-closed-fold-props
       (overlay-get ovr 'vdiff-other-fold)))))

;; * Movement

(defun vdiff--nth-hunk (&optional n use-folds)
  "Return point at Nth hunk in buffer. Use folds instead of hunks
with non-nil USE-FOLDS."
  (let* ((n (or n 1))
         (reverse (< n 0))
         pnt)
    (save-excursion
      (dotimes (_i (abs n))
        ;; Escape current overlay
        (vdiff--maybe-exit-overlay reverse)
        (setq pnt (point))
        ;; Find next overlay
        (while (not (or (and reverse (bobp))
                        (and (not reverse) (eobp))
                        (and use-folds
                             (vdiff--fold-at-point-p))
                        (and (not use-folds)
                             (vdiff--hunk-at-point-p))))
          (setq pnt
                (goto-char (if reverse
                               (previous-overlay-change pnt)
                             (next-overlay-change pnt)))))))
    pnt))

(defun vdiff-next-hunk (arg)
  "Jump to next change in this buffer."
  (interactive "p")
  (let ((count (or arg 1)))
    (goto-char (vdiff--nth-hunk count))
    (vdiff-sync-and-center)))

(defun vdiff-previous-hunk (arg)
  "Jump to previous change in this buffer."
  (interactive "p")
  (let ((count (or (- arg) -1)))
    (goto-char (vdiff--nth-hunk count))
    (vdiff-sync-and-center)))

(defun vdiff-next-fold (arg)
  "Jump to next fold in this buffer."
  (interactive "p")
  (let ((count (or arg 1)))
    (goto-char (vdiff--nth-hunk count t))
    (vdiff-sync-and-center)))

(defun vdiff-previous-fold (arg)
  "Jump to previous fold in this buffer."
  (interactive "p")
  (let ((count (or (- arg) -1)))
    (goto-char (vdiff--nth-hunk count t))
    (vdiff-sync-and-center)))

;; * Entry points

;;;###autoload
(defun vdiff-files (file-a file-b &optional horizontal)
  "Start a vdiff session. If called interactively, you will be
asked to select two files."
  (interactive
   (let* ((file-a (read-file-name "File 1: "))
          (default-directory
            (file-name-directory file-a)))
     (list
      file-a
      (read-file-name
       (format "[File 1 %s] File 2: "
               (file-name-nondirectory file-a)))
      current-prefix-arg)))
  (vdiff-buffers (find-file-noselect file-a)
                 (find-file-noselect file-b)
                 horizontal))

;;;###autoload
(defun vdiff-buffers (buffer-a buffer-b &optional horizontal)
  "Start a vdiff session. If called interactively, you will be
asked to select two buffers."
  (interactive
   (let* ((buffer-a
           (get-buffer
            (read-buffer
             "Buffer 1: " (current-buffer)))))
     (list
      buffer-a
      (get-buffer
       (read-buffer
        (format "[Buffer 1 %s] Buffer 2: " buffer-a)
        (window-buffer (next-window (selected-window)))))
      current-prefix-arg)))
  (delete-other-windows)
  (switch-to-buffer buffer-a)
  (goto-char (point-min))
  (save-selected-window
    (if horizontal
        (split-window-vertically)
      (split-window-horizontally))
    (switch-to-buffer-other-window buffer-b)
    (setq vdiff--buffers (list buffer-a buffer-b))
    (vdiff--with-all-buffers
     (vdiff-mode 1)))
  (vdiff-refresh))

(defun vdiff-quit ()
  "Quit `vdiff-mode' and clean up."
  (interactive)
  (dolist (buf vdiff--buffers)
    (with-current-buffer buf
      (vdiff-mode -1)))
  (message "vdiff exited"))

(defvar vdiff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-l"  'vdiff-sync-and-center)
    map))

(defvar vdiff-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'vdiff-close-fold)
    (define-key map "C" 'vdiff-close-all-folds)
    (define-key map "f" 'vdiff-refine-this-hunk)
    (define-key map "F" 'vdiff-refine-all-hunks)
    (define-key map "g" 'vdiff-switch-buffer)
    (define-key map "h" 'vdiff-maybe-hydra)
    (define-key map "n" 'vdiff-next-hunk)
    (define-key map "N" 'vdiff-next-fold)
    (define-key map "o" 'vdiff-open-fold)
    (define-key map "O" 'vdiff-open-all-folds)
    (define-key map "p" 'vdiff-previous-hunk)
    (define-key map "P" 'vdiff-previous-fold)
    (define-key map "q" 'vdiff-quit)
    (define-key map "r" 'vdiff-receive-changes)
    (define-key map "s" 'vdiff-send-changes)
    (define-key map "x" 'vdiff-remove-refinements-in-hunk)
    (define-key map "t" 'vdiff-close-other-folds)
    (define-key map "u" 'vdiff-refresh)
    (define-key map "w" 'vdiff-save-buffers)
    map))

(defvar vdiff-scroll-lock-mode)

(define-minor-mode vdiff-mode
  "Minor mode active in a vdiff session. This sets up key
bindings in `vdiff-mode-map' and adds hooks to refresh diff on
changes. This will be enabled automatically after calling
commands like `vdiff-files' or `vdiff-buffers'."
  nil " vdiff" 'vdiff-mode-map
  (cond (vdiff-mode
         (setq cursor-in-non-selected-windows nil)
         (add-hook 'after-save-hook #'vdiff-refresh nil t)
         (add-hook 'after-change-functions #'vdiff--after-change-function nil t)
         (add-hook 'pre-command-hook #'vdiff--flag-new-command nil t)
         (when vdiff-lock-scrolling
           (vdiff-scroll-lock-mode 1))
         (setq vdiff--window-configuration
               (current-window-configuration)))
        (t
         (vdiff--remove-all-overlays)
         (setq cursor-in-non-selected-windows t)
         (remove-hook 'after-save-hook #'vdiff-refresh t)
         (remove-hook 'after-change-functions #'vdiff--after-change-function t)
         (remove-hook 'pre-command-hook #'vdiff--flag-new-command t)
         (when vdiff-scroll-lock-mode
           (vdiff-scroll-lock-mode -1))
         (setq vdiff--diff-data nil)
         (setq vdiff--buffers nil)
         (setq vdiff--a-b-line-map nil)
         (setq vdiff--b-a-line-map nil)
         (setq vdiff--window-configuration nil)
         (when (process-live-p vdiff--process-buffer)
           (kill-process vdiff--process-buffer))
         (when (buffer-live-p vdiff--process-buffer)
           (kill-buffer vdiff--process-buffer)))))

(define-minor-mode vdiff-scroll-lock-mode
  "Lock scrolling between vdiff buffers. This minor mode will be
enabled automatically if `vdiff-lock-scrolling' is non-nil."
  nil nil nil
  (cond (vdiff-scroll-lock-mode
         (unless vdiff-mode
           (vdiff-mode 1))
         (vdiff--with-all-buffers
          (add-hook 'window-scroll-functions #'vdiff--scroll-function nil t))
         (message "Scrolling locked"))
        (t
         (vdiff--with-all-buffers
          (remove-hook 'window-scroll-functions #'vdiff--scroll-function t))
         (message "Scrolling unlocked"))))

(defun vdiff--define-hydra ()
  "Define `vdiff-hydra'"
  (defhydra vdiff-hydra (nil nil :hint nil :foreign-keys run)
    (concat (propertize
             "\
 Navigation^^^^          Refine^^   Transmit^^   Folds^^^^            Other^^^^                 "
             'face 'header-line)
            "
 _n_/_N_ next hunk/fold  _f_ this   _s_ send     _o_/_O_ open (all)   _u_ ^ ^ update diff
 _p_/_P_ prev hunk/fold  _F_ all    _r_ receive  _c_/_C_ close (all)  _w_ ^ ^ save buffers
 _g_^ ^  switch buffers  _x_ clear  ^ ^          _t_ ^ ^ close other  _q_/_Q_ quit hydra/vdiff")
    ("n" vdiff-next-hunk)
    ("p" vdiff-previous-hunk)
    ("N" vdiff-next-fold)
    ("P" vdiff-previous-fold)
    ("g" vdiff-switch-buffer)
    ("s" vdiff-send-changes)
    ("r" vdiff-receive-changes)
    ("o" vdiff-open-fold)
    ("O" vdiff-open-all-folds)
    ("c" vdiff-close-fold)
    ("C" vdiff-close-all-folds)
    ("t" vdiff-close-other-folds)
    ("u" vdiff-refresh)
    ("w" vdiff-save-buffers)
    ("f" vdiff-refine-this-hunk)
    ("F" vdiff-refine-all-hunks)
    ("x" vdiff-remove-refinements-in-hunk)
    ("q" nil :exit t)
    ("Q" vdiff-quit :exit t)))

(defun vdiff-maybe-hydra ()
  "Call `vdiff-hydra/body' if defined."
  (interactive)
  (cond ((fboundp 'vdiff-hydra/body)
         (call-interactively 'vdiff-hydra/body))
        ((require 'hydra nil t)
         (vdiff--define-hydra)
         (call-interactively 'vdiff-hydra/body))
        (t
         (message "hydra package not found."))))

(provide 'vdiff)
;;; vdiff.el ends here
