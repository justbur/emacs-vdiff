;;; vdiff.el --- A diff tool similar to  vimdiff -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/justbur/emacs-vdiff
;; Version: 0.1
;; Keywords: diff
;; Package-Requires: ((emacs "24.4") (hydra "0.13.0"))

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
(require 'diff-mode)
(require 'hydra)

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

(defcustom vdiff-diff3-program "diff3"
  "diff3 program to use."
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

(defcustom vdiff-auto-refine nil
  "If non-nil, automatically refine all hunks."
  :group 'vdiff
  :type 'bool)

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
(defvar vdiff--diff3-code-regexp
  "^\\([1-3]\\):\\([0-9]+\\),?\\([0-9]+\\)?\\([adc]\\)")
(defvar vdiff--inhibit-window-switch nil)
(defvar vdiff--inhibit-diff-update nil)
(defvar vdiff--in-scroll-hook nil)
;; (defvar vdiff--in-post-command-hook nil)
(defvar vdiff--line-maps nil)
(defvar vdiff--folds nil)
(defvar vdiff--all-folds-open nil)
(defvar vdiff--setting-vscroll nil)
(defvar vdiff--diff-stale nil)
(defvar vdiff--after-change-timer nil)
(defvar vdiff--after-change-refresh-delay 1)
(defvar vdiff--window-configuration nil)
(defvar vdiff--new-command nil)
(defvar vdiff--last-command nil)
(defvar vdiff--case-args "")
(defvar vdiff--case-options
  '(("Don't ignore case" . "")
    ("Ignore case (-i)" . "-i")))
(defvar vdiff--whitespace-args "")
(defvar vdiff--whitespace-options
  '(("Don't ignore whitespace" . "")
    ("Ignore all whitespace (-w)" . "-w")
    ("Ignore space changes (-b)" . "-b")
    ("Ignore blank lines (-B)" . "-B")))
(defvar vdiff--3way nil)

;; * Utilities

(defun vdiff--maybe-int (str)
  (let ((num (and str (string-to-number str))))
    (when (and (numberp num)
               (>= num 0))
      num)))

(defun vdiff--buffer-a-p ()
  (eq (current-buffer) (car vdiff--buffers)))

(defun vdiff--buffer-b-p ()
  (eq (current-buffer) (cadr vdiff--buffers)))

(defun vdiff--buffer-c-p ()
  (eq (current-buffer) (nth 2 vdiff--buffers)))

(defun vdiff--buffer-p ()
  (cond ((vdiff--buffer-a-p) 'a)
        ((vdiff--buffer-b-p) 'b)
        ((vdiff--buffer-c-p) 'c)))

(defun vdiff--other-buffer ()
  (if (vdiff--buffer-a-p)
      (cadr vdiff--buffers)
    (car vdiff--buffers)))

(defun vdiff--other-window ()
  (get-buffer-window (vdiff--other-buffer)))

(defun vdiff--all-overlays (ovr)
  (nconc
   (list (overlay-get ovr 'vdiff-a-overlay)
         (overlay-get ovr 'vdiff-b-overlay))
   (when vdiff--3way
     (overlay-get ovr 'vdiff-c-overlay))))

(defun vdiff--other-overlays (ovr)
  (delq ovr (vdiff--all-overlays ovr)))

(defun vdiff--read-target (ovr &optional just-one)
  (when vdiff--3way
    (let* ((other-ovrs (vdiff--other-overlays ovr))
           (choices
            (cond ((overlay-get ovr 'vdiff-a)
                   (nconc
                    (list (cons "B" (list (car other-ovrs)))
                          (cons "C" (list (cadr other-ovrs))))
                    (unless just-one
                      (list (cons "B and C" other-ovrs)))))
                  ((overlay-get ovr 'vdiff-b)
                   (nconc
                    (list (cons "A" (car other-ovrs))
                          (cons "C" (cadr other-ovrs)))
                    (unless just-one
                      (list (cons "A and C" other-ovrs)))))
                  ((overlay-get ovr 'vdiff-c)
                   (nconc
                    (list (cons "A" (car other-ovrs))
                          (cons "B" (cadr other-ovrs)))
                    (unless just-one
                      (list (cons "A and B" other-ovrs))))))))
      (cdr-safe
       (assoc-string
        (completing-read "Choose a target buffer(s): "
                         choices)
        choices)))))

(defun vdiff--target-overlays (this-ovr &optional just-one)
  (when (and (overlayp this-ovr)
             (overlay-get this-ovr 'vdiff))
    (let ((target (vdiff--read-target this-ovr just-one)))
      (cond ((and vdiff--3way
                  target)
             (mapcar
              (lambda (tgt)
                (overlay-get
                 this-ovr
                 (intern (format "vdiff-%s-overlay" tgt))))
              target))
            (vdiff--3way
             (user-error "vdiff: No target overlay"))
            ((overlay-get this-ovr 'vdiff-a)
             (list (overlay-get this-ovr 'vdiff-b-overlay)))
            ((overlay-get this-ovr 'vdiff-b)
             (list (overlay-get this-ovr 'vdiff-a-overlay)))))))

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

;; * Toggles

(defun vdiff-toggle-case (command-line-arg)
  "Toggle ignoring of case in diff command."
  (interactive
   (list (cdr-safe
          (assoc-string
           (completing-read "Case options: "
                            vdiff--case-options)
           vdiff--case-options))))
  (setq vdiff--case-args command-line-arg)
  (when vdiff-mode
    (vdiff-refresh)))

(defun vdiff-toggle-whitespace (command-line-arg)
  "Toggle ignoring of whitespace in diff command."
  (interactive
   (list (cdr-safe
          (assoc-string
           (completing-read "Whitespace options: "
                            vdiff--whitespace-options)
           vdiff--whitespace-options))))
  (setq vdiff--whitespace-args command-line-arg)
  (when vdiff-mode
    (vdiff-refresh)))

;; * Main overlay refresh routine

(defun vdiff-refresh ()
  "Asynchronously refresh diff information."
  (interactive)
  (let* ((tmp-a (make-temp-file "vdiff-a-"))
         (tmp-b (make-temp-file "vdiff-b-"))
         (tmp-c (make-temp-file "vdiff-c-"))
         (prgm (if vdiff--3way
                   vdiff-diff3-program
                 vdiff-diff-program))
         (cmd (mapconcat #'identity
                         (nconc
                          (list
                           prgm
                           vdiff-diff-program-args
                           vdiff--whitespace-args
                           vdiff--case-args
                           tmp-a tmp-b)
                          (when vdiff--3way
                            (list tmp-c)))
                         " "))
         (proc (get-buffer-process
                vdiff--process-buffer)))
    (with-current-buffer (car vdiff--buffers)
      (write-region nil nil tmp-a nil 'quietly))
    (with-current-buffer (cadr vdiff--buffers)
      (write-region nil nil tmp-b nil 'quietly))
    (when vdiff--3way
      (with-current-buffer (caddr vdiff--buffers)
        (write-region nil nil tmp-c nil 'quietly)))
    (when proc
      (kill-process proc))
    (with-current-buffer (get-buffer-create vdiff--process-buffer)
      (erase-buffer))
    (setq vdiff--last-command cmd)
    (setq proc (start-process-shell-command
                vdiff--process-buffer
                vdiff--process-buffer
                cmd))
    (process-put proc 'vdiff-tmp-a tmp-a)
    (process-put proc 'vdiff-tmp-b tmp-b)
    (process-put proc 'vdiff-tmp-c tmp-c)
    (set-process-sentinel proc #'vdiff--diff-refresh-1)))

(defun vdiff--encode-range (insert beg &optional end)
  (let* ((beg (vdiff--maybe-int beg))
         (end (vdiff--maybe-int end)))
    (cond ((and end insert)
           (error "vdiff: multi-line range for a or d code"))
          (insert
           (cons (1+ beg) nil))
          (t
           (cons beg (or end beg))))))

(defun vdiff--parse-diff (buf)
  (let (res)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (re-search-forward vdiff--diff-code-regexp nil t)
        (let* ((code (match-string 3))
               a-range
               b-range)
          (push
           (cl-case (string-to-char code)
             (?a (list (vdiff--encode-range
                        t (match-string 1))
                       (vdiff--encode-range
                        nil (match-string 4) (match-string 5))))
             (?d (list (vdiff--encode-range
                        nil (match-string 1) (match-string 2))
                       (vdiff--encode-range
                        t (match-string 4))))
             (?c (list (vdiff--encode-range
                        nil (match-string 1) (match-string 2))
                       (vdiff--encode-range
                        nil (match-string 4) (match-string 5))))
             (t (error "vdiff: Unexpected code in parse-diff")))
           res))))
    (nreverse res)))

(defun vdiff--parse-diff3 (buf)
  (catch 'final-res
    (let (res)
      (with-current-buffer buf
        (goto-char (point-min))
        (let (a-el b-el c-el)
          (while t
            (cond ((looking-at vdiff--diff3-code-regexp)
                   (let* ((file (string-to-number
                                 (match-string-no-properties 1)))
                          (code (match-string-no-properties 4))
                          (range (vdiff--encode-range
                                  (string= code "a")
                                  (match-string-no-properties 2)
                                  (match-string-no-properties 3))))
                     (cl-case file
                       (1 (setq a-el range))
                       (2 (setq b-el range))
                       (3 (setq c-el range)))))
                  ((and a-el
                        (looking-at-p "^===="))
                   (push (list a-el b-el c-el) res)
                   (setq a-el nil)
                   (setq b-el nil)
                   (setq c-el nil))
                  ((eobp)
                   (push (list a-el b-el c-el) res)
                   (throw 'final-res (nreverse res))))
            (forward-line 1)))))))

(defun vdiff--diff-refresh-1 (proc event)
  "This is the sentinel for `vdiff-refresh'. It does the job of
parsing the diff output and triggering the overlay updates."
  (unless vdiff--inhibit-diff-update
    (let ((parse-func (if vdiff--3way
                          'vdiff--parse-diff3
                        'vdiff--parse-diff))
          finished)
      (cond ((string= "finished\n" event)
             ;; means no difference between files
             (setq vdiff--diff-data nil)
             (setq finished t))
            ((string= "exited abnormally with code 1\n" event)
             (setq vdiff--diff-data
                   (funcall parse-func (process-buffer proc)))
             (setq finished t))
            ((string-match-p "exited abnormally with code" event)
             (setq vdiff--diff-data nil)
             (setq finished t)
             (message "vdiff process error: %s" event)))
      (when finished
        (vdiff--refresh-overlays)
        (vdiff--refresh-line-maps)
        (delete-file (process-get proc 'vdiff-tmp-a))
        (delete-file (process-get proc 'vdiff-tmp-b))
        (delete-file (process-get proc 'vdiff-tmp-c))
        (when vdiff-auto-refine
          (vdiff-refine-all-hunks))))
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
         (target-ovrs (vdiff--target-overlays ovr))
         (word-syn (or syntax-code
                       vdiff-default-refinement-syntax-code))
         (not-word-syn (concat "^" word-syn))
         instructions ovr-ins)
    (when (and ovr
               target-ovrs
               (consp (setq instructions
                            (vdiff--diff-words ovr target-ovrs))))
      (dolist (curr-ovr (vdiff--all-overlays))
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
  (dolist (chg-ovr (vdiff--all-overlays))
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

;; * Bitmaps

(define-fringe-bitmap
  'vdiff--vertical-bar
  (make-vector (frame-char-height) #b00100000)
  nil 8 'center)

(define-fringe-bitmap
  'vdiff--top-left-angle
  (vconcat
   [#b00111111]
   (make-vector (1- (frame-char-height))
                #b00100000))
  nil 8 'bottom)

(define-fringe-bitmap
  'vdiff--bottom-left-angle
  (vconcat
   (make-vector (1- (frame-char-height))
                #b00100000)
   [#b00111111])
  nil 8 'top)

(define-fringe-bitmap
  'vdiff--insertion-arrow
  [#b00111111
   #b00011111
   #b00001111
   #b00011111
   #b00111011
   #b01110001
   #b11100000
   #b11000000
   #b10001111]
  nil 8 'top)

;; * Add overlays

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

(defun vdiff--add-diff-overlay (this-len other-len-1 other-len-2)
  (cond ((null other-len-1)
         (vdiff--add-hunk-overlay this-len t))
        ((null this-len)
         (vdiff--add-subtraction-overlay other-len-1))
        (t
         (vdiff--add-hunk-overlay this-len nil (- other-len-1 this-len)))))

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
      (dolist (hunk vdiff--diff-data)
        (let* ((a-range (nth 0 hunk))
               (b-range (nth 1 hunk))
               (c-range (nth 2 hunk))
               (a-beg (car a-range))
               (a-end (cdr a-range))
               (a-post (if a-end (1+ a-end) a-beg))
               (a-len (when a-end (1+ (- a-end a-beg))))
               (b-beg (car b-range))
               (b-end (cdr b-range))
               (b-post (if b-end (1+ b-end) b-beg))
               (b-insert (null b-end))
               (b-len (when b-end (1+ (- b-end b-beg))))
               c-len)

          (push (cons (cons a-last-post (1- a-beg))
                      (cons b-last-post (1- b-beg)))
                folds)
          (setq a-last-post a-post)
          (setq b-last-post b-post)

          (let (ovr-a ovr-b ovr-c)
            (with-current-buffer a-buffer
              (forward-line (- a-beg a-line))
              (setq a-line a-beg)
              (setq ovr-a (vdiff--add-diff-overlay a-len b-len c-len)))
            (with-current-buffer b-buffer
              (forward-line (- b-beg b-line))
              (setq b-line b-beg)
              (setq ovr-b (vdiff--add-diff-overlay b-len a-len c-len)))
            (when vdiff--3way
              (with-current-buffer b-buffer
                (forward-line (- c-beg c-line))
                (setq c-line c-beg)
                (setq ovr-c (vdiff--add-diff-overlay c-len a-len b-len))))
            (overlay-put ovr-a 'vdiff-a t)
            (overlay-put ovr-a 'vdiff-a-overlay ovr-a)
            (overlay-put ovr-a 'vdiff-b-overlay ovr-b)
            (overlay-put ovr-a 'vdiff-c-overlay ovr-c)
            (overlay-put ovr-b 'vdiff-b t)
            (overlay-put ovr-b 'vdiff-a-overlay ovr-a)
            (overlay-put ovr-b 'vdiff-b-overlay ovr-b)
            (overlay-put ovr-b 'vdiff-c-overlay ovr-c)
            (when vdiff--3way
              (overlay-put ovr-c 'vdiff-c t)
              (overlay-put ovr-c 'vdiff-a-overlay ovr-a)
              (overlay-put ovr-c 'vdiff-b-overlay ovr-b)))))
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

(defun vdiff-send-changes (beg end &optional receive targets)
  "Send changes in this hunk to other vdiff buffer. If the region
is active, send all changes found in the region. Otherwise use
the hunk under point or on the immediately preceding line."
  (interactive (vdiff--region-or-close-overlay))
  (let* ((vdiff--inhibit-diff-update t)
         target-ovrs)
    (dolist (ovr (overlays-in beg end))
      (cond ((and (setq target-ovrs
                        (or targets (vdiff--target-overlays ovr t)))
                  receive)
             ;; Assume that
             (let ((pos (overlay-start (car target-ovrs))))
               (vdiff--with-other-window
                (vdiff-send-changes pos (1+ pos)))))
            ((memq (overlay-get ovr 'vdiff-type)
                   '(change addition))
             (vdiff--transmit-change ovr targets))
            ((eq (overlay-get ovr 'vdiff-type) 'subtraction)
             (vdiff--transmit-subtraction ovr targets))))
    (vdiff-refresh)
    (vdiff--scroll-function)))

(defun vdiff-receive-changes (beg end)
  "Receive the changes corresponding to this position from the
other vdiff buffer. If the region is active, receive all
corresponding changes found in the region. Otherwise use the
changes under point or on the immediately preceding line."
  (interactive (vdiff--region-or-close-overlay))
  (vdiff-send-changes beg end t))

(defun vdiff--transmit-change (ovr &optional targets)
  "Send text in OVR to corresponding overlay in other buffer."
  (if (not (overlayp ovr))
      (message "No change found")
    (let* ((addition (eq 'addition (overlay-get ovr 'vdiff-type)))
           (target-ovrs (or targets (vdiff--target-overlays ovr)))
           (text (buffer-substring-no-properties
                  (overlay-start ovr)
                  (overlay-end ovr))))
      (dolist (target target-ovrs)
        (with-current-buffer (overlay-buffer target)
          (save-excursion
            (goto-char (overlay-start target))
            (unless addition
              (delete-region (overlay-start target)
                             (overlay-end target)))
            (insert text))
          (delete-overlay target)))
      (delete-overlay ovr))))

(defun vdiff--transmit-subtraction (ovr &optional targets)
  "Same idea as `vdiff--transmit-change' except we are
just deleting text in the other buffer."
  (if (not (overlayp ovr))
      (message "No change found")
    (let* ((target-ovrs (or targets
                            (vdiff--target-overlays ovr))))
      (dolist (target target-ovrs)
        (with-current-buffer (overlay-buffer target)
          (delete-region (overlay-start target)
                         (overlay-end target))
          (delete-overlay target))))))

;; * Scrolling and line syncing

(defun vdiff--2way-entries (a-prior a-end a-post b-prior b-end b-post)
  ;; a-prior  0     0 b-prior
  ;; l-beg    1 +   1 b-beg
  ;; l-beg    2 +   2 b-end
  ;;          3 +   -
  ;;          4 +   -
  ;; l-end    5 +   -
  ;; a-post   6     3 b-post
  (let* (a-entries b-entries)
    (dotimes (offset (1+ (max (- a-post a-prior)
                              (- b-post b-prior))))
      (let ((a-line (+ a-prior offset))
            (b-line (+ b-prior offset)))
        (cond ((= offset 0)
               (push (list a-line b-line 0) a-entries)
               (push (list b-line a-line 0) b-entries))
              ((and a-end b-end
                    (<= b-line b-end)
                    (<= a-line a-end))
               (push (list a-line b-line 0) a-entries)
               (push (list b-line a-line 0) b-entries))
              ((and (or (null a-end) (> a-line a-end))
                    (<= b-line b-post))
               (push (list b-line a-post (- a-line (or a-end a-prior) 1)) b-entries))
              ((and (or (null b-end) (> b-line b-end))
                    (<= a-line a-post))
               (push (list a-line b-post (- b-line (or b-end b-prior) 1)) a-entries)))))
    (push (list (1+ a-post) (1+ b-post) 0) a-entries)
    (push (list (1+ b-post) (1+ a-post) 0) b-entries)
    (cons (nreverse a-entries) (nreverse b-entries))))

(defun vdiff--set-cons (vars expr)
  (setf (car vars) (car expr))
  (setf (cdr vars) (cdr expr)))

(defun vdiff--refresh-line-maps ()
  "Sync information in `vdiff--line-map' with
`vdiff--diff-data'."
  (let ((vdiff--inhibit-diff-update t)
        a-b b-a a-c c-a b-c c-b)
    (dolist (hunk vdiff--diff-data)
      (let* ((a-lines (nth 0 hunk))
             (a-beg (car a-lines))
             (a-prior (1- a-beg))
             (a-end (cdr a-lines))
             (a-post (if a-end (1+ a-end) a-beg))
             (b-lines (nth 1 hunk))
             (b-beg (car b-lines))
             (b-prior (1- b-beg))
             (b-end (cdr b-lines))
             (b-post (if b-end (1+ b-end) b-beg))
             (c-lines (nth 2 hunk))
             c-beg c-end c-prior c-post c-len)
        (let ((new-a-b
               (vdiff--2way-entries a-prior a-end a-post b-prior b-end b-post)))
          (setq a-b (nconc a-b (car new-a-b)))
          (setq b-a (nconc b-a (cdr new-a-b)))
          (when c-lines
            (let* ((c-beg (car c-lines))
                   (c-prior (1- c-beg))
                   (c-end (cdr c-lines))
                   (c-post (if c-end (1+ c-end) c-beg))
                   (new-a-c
                    (vdiff--2way-entries a-prior a-end a-post c-prior c-end c-post))
                   (new-b-c
                    (vdiff--2way-entries b-prior b-end b-post c-prior c-end c-post)))
              (setq a-c (nconc a-c (car new-a-c)))
              (setq c-a (nconc c-a (cdr new-a-c)))
              (setq b-c (nconc b-c (car new-b-c)))
              (setq c-b (nconc c-b (cdr new-b-c))))))))
    (setq vdiff--line-maps
          (if vdiff--3way
              (list (list 'a a-b a-c)
                    (list 'b b-a b-c)
                    (list 'c c-a c-b))
            (list (list 'a a-b)
                  (list 'b b-a))))))

(defun vdiff--translate-line (line &optional from-buffer)
  "Translate LINE in buffer A to corresponding line in buffer
B. Go from buffer B to A if B-to-A is non nil."
  (interactive (list (line-number-at-pos)))
  (let* ((from-buffer (or from-buffer (vdiff--buffer-p)))
         (maps (cdr (assq from-buffer vdiff--line-maps)))
         last-entry res-1 res-2)
    (dolist (map maps)
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
      (if res-1
          (setq res-2 (cons (+ (- line (car last-entry)) (cadr last-entry))
                            (nth 2 last-entry)))
        (setq res-1 (cons (+ (- line (car last-entry)) (cadr last-entry))
                          (nth 2 last-entry)))))
    (when (called-interactively-p 'interactive)
      (message "This line: %s; Other line %s; vscroll-state %s; entry %s"
               line res-1 (cdr res-1) last-entry))
    (cons res-1 res-2)))

(defun vdiff-switch-buffer (line)
  "Jump to the line in the other vdiff buffer that corresponds to
the current one."
  (interactive (list (line-number-at-pos)))
  (vdiff-refresh)
  (let ((line (caar (vdiff--translate-line line))))
    (select-window (vdiff--other-window))
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
  (let ((new-line (caar (vdiff--translate-line line))))
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
              (vdiff--translate-line this-start-line))
             (other-curr-start (window-start other-window))
             (other-start-line (caar start-translation))
             (other-start-pos (when other-start-line
                                (vdiff--pos-at-line-beginning
                                 other-start-line other-buffer)))
             (scroll-amt (cdar start-translation))
             (this-line (+ (count-lines window-start (point))
                           this-start-line))
             (translation (vdiff--translate-line this-line))
             (other-pos (when translation
                          (vdiff--pos-at-line-beginning
                           (caar translation) other-buffer)))
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
                " " 'display '(left-fringe vdiff--vertical-bar)))
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
  (save-excursion
    (setq vdiff--all-folds-open t)
    (vdiff-open-fold (point-min) (point-max))))

(defun vdiff-close-all-folds ()
  "Close all folds in both buffers"
  (interactive)
  (save-excursion
    (setq vdiff--all-folds-open nil)
    (vdiff-close-fold (point-min) (point-max))))

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
    (define-key map "h" 'vdiff-hydra/body)
    (define-key map "ic" 'vdiff-toggle-case)
    (define-key map "iw" 'vdiff-toggle-whitespace)
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
         (setq vdiff--line-maps nil)
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

(defun vdiff--current-case ()
  (if (string= "" vdiff--case-args) "off" "on (-i)"))

(defun vdiff--current-whitespace ()
  (pcase vdiff--whitespace-args
    ("" "off")
    ("-w" "all (-w)")
    ("-b" "space changes (-b)")
    ("-B" "blank lines (-B)")))

(defhydra vdiff-toggle-hydra (nil nil :hint nil)
  (concat (propertize
           "\
 Toggles"
           'face 'header-line)
          "
 _c_ ignore case (current: %s(vdiff--current-case))
 _w_ ignore whitespace (current: %s(vdiff--current-whitespace))
 _q_ back to main hydra")

  ("c" vdiff-toggle-case)
  ("w" vdiff-toggle-whitespace)
  ("q" vdiff-hydra/body :exit t))

(defhydra vdiff-hydra (nil nil :hint nil :foreign-keys run)
  (concat (propertize
           "\
 Navigation^^^^          Refine^^   Transmit^^   Folds^^^^            Other^^^^                 "
           'face 'header-line)
          "
 _n_/_N_ next hunk/fold  _f_ this   _s_ send     _o_/_O_ open (all)   _i_ ^ ^ toggles
 _p_/_P_ prev hunk/fold  _F_ all    _r_ receive  _c_/_C_ close (all)  _u_ ^ ^ update diff
 _g_^ ^  switch buffers  _x_ clear  ^ ^          _t_ ^ ^ close other  _w_ ^ ^ save buffers
 ^ ^^ ^                  ^ ^        ^ ^          ^ ^ ^ ^              _q_/_Q_ quit hydra/vdiff
 ignore case: %s(vdiff--current-case) | ignore whitespace: %s(vdiff--current-whitespace)")
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
  ("i" vdiff-toggle-hydra/body :exit t)
  ("q" nil :exit t)
  ("Q" vdiff-quit :exit t))

(provide 'vdiff)
;;; vdiff.el ends here
