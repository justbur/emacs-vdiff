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

(defcustom vdiff-diff-extra-args ""
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

(defface vdiff-target-face
  '((t :inverse-video t :inherit warning))
  "Face for selecting hunk targets."
  :group 'vdiff)

(defvar vdiff--force-sync-commands '(next-line
                                     previous-line
                                     beginning-of-buffer
                                     end-of-buffer)
  "Commands that trigger sync in other buffer. There should not
be a need to include commands that scroll the buffer here,
because those are handled differently.")

(defvar vdiff--diff-code-regexp
  "^\\([0-9]+\\),?\\([0-9]+\\)?\\([adc]\\)\\([0-9]+\\),?\\([0-9]+\\)?")
(defvar vdiff--diff3-code-regexp
  "^\\([1-3]\\):\\([0-9]+\\),?\\([0-9]+\\)?\\([adc]\\)")
(defvar vdiff--inhibit-window-switch nil)
(defvar vdiff--inhibit-diff-update nil)
(defvar vdiff--in-scroll-hook nil)
;; (defvar vdiff--in-post-command-hook nil)
(defvar vdiff--setting-vscroll nil)
(defvar vdiff--after-change-timer nil)
(defvar vdiff--after-change-refresh-delay 1)
(defvar vdiff--new-command nil)
;; (defvar vdiff--last-command nil)
(defvar vdiff--case-options
  '(("Don't ignore case" . "")
    ("Ignore case (-i)" . "-i")))
(defvar vdiff--whitespace-options
  '(("Don't ignore whitespace" . "")
    ("Ignore all whitespace (-w)" . "-w")
    ("Ignore space changes (-b)" . "-b")
    ("Ignore blank lines (-B)" . "-B")))

;; Sessions
(defvar vdiff--temp-session nil)
(defvar-local vdiff--session nil)
(cl-defstruct vdiff-session
  ;; buffers
  buffers
  process-buffer
  word-diff-output-buffer
  ;; data
  diff-data
  line-maps
  folds
  all-folds-open
  diff-stale
  ;; other
  window-config
  case-args
  whitespace-args)


;; * Utilities

(defun vdiff--maybe-int (str)
  (let ((num (and str (string-to-number str))))
    (when (and (numberp num)
               (>= num 0))
      num)))

(defun vdiff--non-nil-list (&rest args)
  (delq nil (apply #'list args)))

(defun vdiff--buffer-a-p ()
  (when (and
         vdiff--session
         (eq (current-buffer)
             (car (vdiff-session-buffers vdiff--session))))
    (current-buffer)))

(defun vdiff--buffer-b-p ()
  (when (and
         vdiff--session
         (eq (current-buffer)
             (cadr (vdiff-session-buffers vdiff--session))))
    (current-buffer)))

(defun vdiff--buffer-c-p ()
  (when (and
         vdiff--session
         (eq (current-buffer)
             (nth 2 (vdiff-session-buffers vdiff--session))))
    (current-buffer)))

(defun vdiff--buffer-p ()
  (cond ((vdiff--buffer-a-p) 'a)
        ((vdiff--buffer-b-p) 'b)
        ((vdiff--buffer-c-p) 'c)))

(defun vdiff--unselected-buffers ()
  (remq (current-buffer)
        (vdiff-session-buffers vdiff--session)))

(defun vdiff--unselected-windows ()
  (mapcar #'get-buffer-window
          (vdiff--unselected-buffers)))

(defun vdiff--all-windows ()
  (remq nil
        (mapcar #'get-buffer-window
                (vdiff-session-buffers vdiff--session))))

(defun vdiff--all-overlays (ovr)
  (overlay-get ovr 'vdiff-hunk-overlays))

(defun vdiff--other-overlays (ovr)
  (remq ovr (vdiff--all-overlays ovr)))

(defun vdiff--overlay-marker (ovr)
  (let ((current (eq (current-buffer) (overlay-buffer ovr))))
    (propertize
     (format "%s%s\n"
             (1+
              (cl-position
               (overlay-buffer ovr)
               (vdiff-session-buffers vdiff--session)))
             (if current " (to all) " ""))
     'face 'vdiff-target-face)))

(defun vdiff--add-overlay-marker (ovr)
  (overlay-put ovr 'before-string
               (concat (vdiff--overlay-marker ovr)
                       (overlay-get ovr 'before-string))))

(defun vdiff--remove-overlay-marker (ovr)
  (overlay-put ovr 'before-string
               (substring
                (overlay-get ovr 'before-string)
                (length (vdiff--overlay-marker ovr)))))

(defun vdiff--read-3way-target (ovr &optional just-one)
  (when vdiff-3way-mode
    (let* ((all-ovrs (vdiff--all-overlays ovr))
           (other-ovrs (remq ovr all-ovrs))
           (this-idx (cl-position (vdiff--overlay-at-pos)
                                  all-ovrs))
           (marked-ovrs (if just-one other-ovrs all-ovrs))
           target)
      (unwind-protect
          (progn
            (mapc #'vdiff--add-overlay-marker marked-ovrs)
            (setq target (1- (string-to-number
                              (char-to-string
                               (read-char "Select target: ")))))
            (cond ((or (not (member target (list 0 1 2)))
                       (and just-one (= target this-idx)))
                   (user-error "Invalid target"))
                  ((= target this-idx)
                   (message "all others %s %s" target this-idx)
                   other-ovrs)
                  (t
                   (message "just %s" (nth target all-ovrs))
                   (list (nth target all-ovrs)))))
        (mapc #'vdiff--remove-overlay-marker marked-ovrs)))))

(defun vdiff--target-overlays (this-ovr &optional just-one)
  (when (and (overlayp this-ovr)
             (overlay-get this-ovr 'vdiff))
    (let ((3way-target (vdiff--read-3way-target this-ovr just-one))
          (other-ovrs (vdiff--other-overlays this-ovr)))
      (cond ((and vdiff-3way-mode
                  3way-target)
             (cl-remove-if
              (lambda (ovr)
                (or (eq ovr this-ovr)
                    (null (member ovr 3way-target))))
              other-ovrs))
            (vdiff-3way-mode
             (user-error "vdiff: No target overlay"))
            (t
             (remq this-ovr other-ovrs))))))

(defun vdiff--min-window-width ()
  (apply #'min
         (mapcar (lambda (buf)
                   (window-width (get-buffer-window buf)))
                 (vdiff-session-buffers vdiff--session))))

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

(defmacro vdiff--with-all-buffers (&rest body)
  "Execute BODY in all vdiff buffers."
  `(dolist (buf (vdiff-session-buffers vdiff--session))
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
  (setf (vdiff-session-case-args vdiff--session)
        command-line-arg)
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
  (setf (vdiff-session-whitespace-args vdiff--session)
        command-line-arg)
  (when vdiff-mode
    (vdiff-refresh)))

;; * Main overlay refresh routine

(defun vdiff-refresh ()
  "Asynchronously refresh diff information."
  (interactive)
  (when (vdiff--buffer-p)
    (let* ((tmp-a (make-temp-file "vdiff-a-"))
           (tmp-b (make-temp-file "vdiff-b-"))
           (tmp-c (when vdiff-3way-mode
                    (make-temp-file "vdiff-c-")))
           (prgm (if vdiff-3way-mode
                     vdiff-diff3-program
                   vdiff-diff-program))
           (ses vdiff--session)
           (cmd (mapconcat
                 #'identity
                 (vdiff--non-nil-list
                  prgm
                  (vdiff-session-whitespace-args ses)
                  (vdiff-session-case-args ses)
                  vdiff-diff-extra-args
                  tmp-a tmp-b tmp-c)
                 " "))
           (buffers (vdiff-session-buffers ses))
           (proc-buf (vdiff-session-process-buffer ses))
           (proc (get-buffer-process proc-buf)))
      (setq vdiff--last-command cmd)
      (with-current-buffer (car buffers)
        (write-region nil nil tmp-a nil 'quietly))
      (with-current-buffer (cadr buffers)
        (write-region nil nil tmp-b nil 'quietly))
      (when vdiff-3way-mode
        (with-current-buffer (nth 2 buffers)
          (write-region nil nil tmp-c nil 'quietly)))
      (when proc
        (kill-process proc))
      (with-current-buffer (get-buffer-create proc-buf)
        (erase-buffer))
      ;; (setq vdiff--last-command cmd)
      (setq proc
            (start-process-shell-command proc-buf proc-buf cmd))
      (when vdiff-3way-mode
        (process-put proc 'vdiff-3way t))
      (process-put proc 'vdiff-session ses)
      (process-put proc 'vdiff-tmp-a tmp-a)
      (process-put proc 'vdiff-tmp-b tmp-b)
      (when tmp-c
        (process-put proc 'vdiff-tmp-c tmp-c))
      (set-process-sentinel proc #'vdiff--diff-refresh-1))))

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
        (let* ((code (match-string 3)))
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
                   (when (or a-el b-el)
                     (push (list a-el b-el c-el) res))
                   (throw 'final-res (nreverse res))))
            (forward-line 1)))))))

(defun vdiff--diff-refresh-1 (proc event)
  "This is the sentinel for `vdiff-refresh'. It does the job of
parsing the diff output and triggering the overlay updates."
  (unless vdiff--inhibit-diff-update
    (let ((parse-func (if (process-get proc 'vdiff-3way)
                          'vdiff--parse-diff3
                        'vdiff--parse-diff))
          (ses (process-get proc 'vdiff-session))
          finished)
      (cond
       ;; Was getting different exit code conventions depending on the
       ;; version of diff used
       ((or (string= "finished\n" event)
            (string= "exited abnormally with code 1\n" event))
        (setf (vdiff-session-diff-data ses)
              (funcall parse-func (process-buffer proc)))
        (setq finished t))
       ((string-match-p "exited abnormally with code" event)
        (setf (vdiff-session-diff-data ses) nil)
        (setq finished t)
        (message "vdiff process error: %s" event)))
      (when finished
        (vdiff--refresh-overlays)
        (vdiff--refresh-line-maps)
        (delete-file (process-get proc 'vdiff-tmp-a))
        (delete-file (process-get proc 'vdiff-tmp-b))
        (when (process-get proc 'vdiff-tmp-c)
          (delete-file (process-get proc 'vdiff-tmp-c)))
        (when vdiff-auto-refine
          (vdiff-refine-all-hunks)))
      (setf (vdiff-session-diff-stale ses) nil))))

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
         (target-ovr (car (vdiff--target-overlays ovr)))
         (word-syn (or syntax-code
                       vdiff-default-refinement-syntax-code))
         (not-word-syn (concat "^" word-syn))
         instructions ovr-ins)
    (when (and ovr
               target-ovr
               (consp (setq instructions
                            (vdiff--diff-words ovr target-ovr))))
      (dolist (curr-ovr (vdiff--all-overlays ovr))
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
  (dolist (chg-ovr (vdiff--all-overlays ovr))
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
  ;; Doesn't work for diff3 yet
  (when vdiff-mode
    (dolist (ovr (overlays-in (point-min) (point-max)))
      (vdiff-refine-this-hunk syntax-code ovr))))

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

(defun vdiff--add-folds (a-buffer b-buffer c-buffer folds)
  (let (new-folds)
    (dolist (fold folds)
      (let ((a-range (vdiff--narrow-fold-range (nth 0 fold)))
            (b-range (vdiff--narrow-fold-range (nth 1 fold)))
            (c-range (when c-buffer
                       (vdiff--narrow-fold-range (nth 2 fold)))))
        (cond
         ;; ((assoc a-range vdiff--folds)
         ;;  ;; Restore any overlays on same range
         ;;  (let* ((a-fold (cadr (assoc a-range vdiff--folds)))
         ;;         (b-fold (cl-caddr (assoc a-range vdiff--folds)))
         ;;         (a-beg (vdiff--pos-at-line-beginning
         ;;                 (car a-range) a-buffer))
         ;;         (a-end (vdiff--pos-at-line-beginning
         ;;                 (cdr a-range) a-buffer))
         ;;         (b-beg (vdiff--pos-at-line-beginning
         ;;                 (car b-range) b-buffer))
         ;;         (b-end (vdiff--pos-at-line-beginning
         ;;                 (cdr b-range) b-buffer)))
         ;;    (move-overlay a-fold a-beg a-end a-buffer)
         ;;    (move-overlay b-fold b-beg b-end b-buffer)
         ;;    (push (list a-range a-fold b-fold) new-folds)))
         ((> (1+ (- (cdr a-range) (car a-range))) vdiff-min-fold-size)
          ;; Ranges include padding
          (let ((a-fold (vdiff--make-fold a-buffer a-range))
                (b-fold (vdiff--make-fold b-buffer b-range))
                (c-fold (when c-buffer
                          (vdiff--make-fold c-buffer c-range))))
            (dolist (fold (list a-fold b-fold c-fold))
              (when fold
                (cond ((or (vdiff--point-in-fold-p a-fold)
                           (vdiff--point-in-fold-p b-fold)
                           (and c-fold
                                (vdiff--point-in-fold-p c-fold))
                           (vdiff-session-all-folds-open
                            vdiff--session))
                       (vdiff--set-open-fold-props fold))
                      (t
                       (vdiff--set-closed-fold-props fold)))))
            (overlay-put a-fold 'vdiff-other-folds
                         (vdiff--non-nil-list b-fold c-fold))
            (overlay-put b-fold 'vdiff-other-folds
                         (vdiff--non-nil-list a-fold c-fold))
            (when c-fold
              (overlay-put c-fold 'vdiff-other-folds (list a-fold b-fold)))
            (push (vdiff--non-nil-list a-range a-fold b-fold c-fold)
                  new-folds))))))
    (setf (vdiff-session-folds vdiff--session) new-folds)))

(defun vdiff--remove-fold-overlays (_)
  (setf (vdiff-session-folds vdiff--session) nil))

(defun vdiff--add-diff-overlay (this-len other-len-1 other-len-2)
  (let ((max-other-len (max (if other-len-1 other-len-1 0)
                            (if other-len-2 other-len-2 0))))
    (cond ((and (null other-len-1) (null other-len-2))
           (vdiff--add-hunk-overlay this-len t))
          ((null this-len)
           (vdiff--add-subtraction-overlay max-other-len))
          (t
           (vdiff--add-hunk-overlay this-len nil
                                    (- max-other-len this-len))))))

(defun vdiff--refresh-overlays ()
  "Delete and recreate overlays in both buffers."
  (vdiff--remove-all-overlays)
  (let ((a-buffer (car (vdiff-session-buffers vdiff--session)))
        (b-buffer (cadr (vdiff-session-buffers vdiff--session)))
        (c-buffer (nth 2 (vdiff-session-buffers vdiff--session)))
        (a-line 1)
        (b-line 1)
        (c-line 1)
        (a-last-post 1)
        (b-last-post 1)
        (c-last-post 1)
        (vdiff--inhibit-diff-update t)
        folds)
    (save-excursion
      (with-current-buffer a-buffer
        (widen)
        (goto-char (point-min)))
      (with-current-buffer b-buffer
        (widen)
        (goto-char (point-min)))
      (when c-buffer
        (with-current-buffer c-buffer
          (widen)
          (goto-char (point-min))))
      (dolist (hunk (vdiff-session-diff-data vdiff--session))
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
               (b-len (when b-end (1+ (- b-end b-beg))))
               c-beg c-end c-post c-len)
          (when c-buffer
            (setq c-beg (car c-range))
            (setq c-end (cdr c-range))
            (setq c-post (if c-end (1+ c-end) c-beg))
            (setq c-len (when c-end (1+ (- c-end c-beg)))))

          (push (list (cons a-last-post (1- a-beg))
                      (cons b-last-post (1- b-beg))
                      (when c-beg
                        (cons c-last-post (1- c-beg))))
                folds)
          (setq a-last-post a-post)
          (setq b-last-post b-post)
          (when c-buffer
            (setq c-last-post c-post))

          (let (ovr-a ovr-b ovr-c)
            (with-current-buffer a-buffer
              (forward-line (- a-beg a-line))
              (setq a-line a-beg)
              (setq ovr-a (vdiff--add-diff-overlay a-len b-len c-len)))
            (with-current-buffer b-buffer
              (forward-line (- b-beg b-line))
              (setq b-line b-beg)
              (setq ovr-b (vdiff--add-diff-overlay b-len a-len c-len)))
            (when c-buffer
              (with-current-buffer c-buffer
                (forward-line (- c-beg c-line))
                (setq c-line c-beg)
                (setq ovr-c (vdiff--add-diff-overlay c-len a-len b-len))))
            (let ((ovr-group (vdiff--non-nil-list ovr-a ovr-b ovr-c)))
              (overlay-put ovr-a 'vdiff-a t)
              (overlay-put ovr-a 'vdiff-hunk-overlays ovr-group)
              (overlay-put ovr-b 'vdiff-b t)
              (overlay-put ovr-b 'vdiff-hunk-overlays ovr-group)
              (when c-buffer
                (overlay-put ovr-c 'vdiff-c t)
                (overlay-put ovr-c 'vdiff-hunk-overlays ovr-group))))))
      (push (list (cons a-last-post
                        (with-current-buffer a-buffer
                          (line-number-at-pos (point-max))))
                  (cons b-last-post
                        (with-current-buffer b-buffer
                          (line-number-at-pos (point-max))))
                  (when c-buffer
                    (cons c-last-post
                          (with-current-buffer c-buffer
                            (line-number-at-pos (point-max))))))
            folds))
    (vdiff--add-folds a-buffer b-buffer c-buffer folds)))

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

(defun vdiff-send-changes (beg end &optional receive targets dont-refresh)
  "Send changes in this hunk to another vdiff buffer. If the
region is active, send all changes found in the region. Otherwise
use the hunk under point or on the immediately preceding line."
  (interactive (vdiff--region-or-close-overlay))
  (let* ((vdiff--inhibit-diff-update t)
         target-ovrs)
    (dolist (ovr (overlays-in beg end))
      (cond ((and receive
                  (setq target-ovrs
                        (or targets (vdiff--target-overlays ovr t))))
             (let ((pos (overlay-start (car target-ovrs))))
               (with-current-buffer (overlay-buffer (car target-ovrs))
                 (vdiff-send-changes pos (1+ pos) nil nil t))))
            ((memq (overlay-get ovr 'vdiff-type)
                   '(change addition))
             (vdiff--transmit-change ovr targets))
            ((eq (overlay-get ovr 'vdiff-type) 'subtraction)
             (vdiff--transmit-subtraction ovr targets))))
    (unless dont-refresh
      (vdiff-refresh)
      (vdiff--scroll-function))))

(defun vdiff-receive-changes (beg end)
  "Receive the changes corresponding to this position from
another vdiff buffer. This is equivalent to jumping to the
corresponding buffer and sending from there. If the region is
active, receive all corresponding changes found in the
region. Otherwise use the changes under point or on the
immediately preceding line."
  (interactive (vdiff--region-or-close-overlay))
  (vdiff-send-changes beg end t))

(defun vdiff--transmit-change (ovr &optional targets)
  "Send text in OVR to corresponding overlay in other buffer."
  (if (not (overlayp ovr))
      (message "No change found")
    (let* ((target-ovrs (or targets (vdiff--target-overlays ovr)))
           (text (buffer-substring-no-properties
                  (overlay-start ovr)
                  (overlay-end ovr))))
      (dolist (target target-ovrs)
        (with-current-buffer (overlay-buffer target)
          (save-excursion
            (goto-char (overlay-start target))
            ;; subtractions are one char too big on purpose
            (unless (eq (overlay-get target 'vdiff-type)
                        'subtraction)
              (delete-region (overlay-start target)
                             (overlay-end target)))
            (insert text))
          (delete-overlay target)))
      (delete-overlay ovr))))

(defun vdiff--transmit-subtraction (ovr &optional targets)
  "Same idea as `vdiff--transmit-change' except we are
just deleting text in another buffer."
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
        (a-b (list (list 0 0 0)))
        (b-a (list (list 0 0 0)))
        (a-c (list (list 0 0 0)))
        (c-a (list (list 0 0 0)))
        (b-c (list (list 0 0 0)))
        (c-b (list (list 0 0 0))))
    (dolist (hunk (vdiff-session-diff-data vdiff--session))
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
             (c-lines (nth 2 hunk)))
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
    (setf (vdiff-session-line-maps vdiff--session)
          (if vdiff-3way-mode
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
         (maps
          (cdr
           (assq from-buffer (vdiff-session-line-maps vdiff--session))))
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
        (message "Error in line translation %s %s" line from-buffer))
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
  "Jump to the line in another vdiff buffer that corresponds to
the current one."
  (interactive (list (line-number-at-pos)))
  (vdiff-refresh)
  (let ((line (caar (vdiff--translate-line line))))
    (select-window (car (vdiff--unselected-windows)))
    (when line
      (vdiff--move-to-line line))))

(defun vdiff--recenter-all ()
  (dolist (win (vdiff--all-windows))
    (with-selected-window win
      (recenter))))

(defun vdiff-sync-and-center ()
  "Sync point in another vdiff buffers to the line in this
buffer and recenter all buffers."
  (interactive)
  (vdiff--scroll-function)
  (vdiff--recenter-all))

(defun vdiff-restore-windows ()
  "Restore initial window configuration."
  (interactive)
  (set-window-configuration
   (vdiff-session-window-config vdiff--session)))

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
         (when (and vscroll
                    (eq vdiff-subtraction-style 'full))
           (set-window-vscroll window vscroll))
         (force-window-update window))))))

(defun vdiff--flag-new-command ()
  (setq vdiff--new-command t))

(defun vdiff--other-win-scroll-data (_window window-start &optional buf-c)
  ;; need other-win, start-pos, pos and scroll-amt
  (let* ((other-buf (nth (if buf-c 1 0) (vdiff--unselected-buffers)))
         (other-win (nth (if buf-c 1 0) (vdiff--unselected-windows)))
         (start-line (line-number-at-pos window-start))
         (start-trans (vdiff--translate-line start-line))
         (start-trans (if buf-c
                          (cdr start-trans)
                        (car start-trans)))
         (trans (vdiff--translate-line
                 (+ (count-lines window-start (point))
                    start-line)))
         (trans (if buf-c (cdr trans) (car trans))))
    (when (and start-trans trans)
      (list other-win
            (vdiff--pos-at-line-beginning (car start-trans) other-buf)
            (vdiff--pos-at-line-beginning (car trans) other-buf)
            (cdr start-trans)))))

(defun vdiff--scroll-function (&optional window window-start)
  "Sync scrolling of all vdiff windows."
  (let* ((window (or window (selected-window)))
         (window-start (or window-start (window-start))))
    (when (and (eq window (selected-window))
               (cl-every #'window-live-p (vdiff--all-windows))
               (vdiff--buffer-p)
               (not vdiff--in-scroll-hook)
               vdiff--new-command)
      (setq vdiff--new-command nil)
      (let* ((2-scroll-data (vdiff--other-win-scroll-data
                             window window-start))
             (2-win (nth 0 2-scroll-data))
             (2-start-pos (nth 1 2-scroll-data))
             (2-pos (nth 2 2-scroll-data))
             (2-scroll (nth 3 2-scroll-data))
             ;; 1 is short for this; 2 is the first other and 3 is the second
             (vdiff--in-scroll-hook t))
        (when (and 2-pos 2-start-pos)
          (set-window-point 2-win 2-pos)
          (set-window-start 2-win 2-start-pos)
          (vdiff--set-vscroll-and-force-update 2-win 2-scroll))
        (when vdiff-3way-mode
          (let*
              ((3-scroll-data (vdiff--other-win-scroll-data
                               window window-start t))
               (3-win (nth 0 3-scroll-data))
               (3-start-pos (nth 1 3-scroll-data))
               (3-pos (nth 2 3-scroll-data))
               (3-scroll (nth 3 3-scroll-data)))
            (when (and 3-start-pos 3-pos)
              (set-window-point 3-win 3-pos)
              (set-window-start 3-win 3-start-pos)
              (vdiff--set-vscroll-and-force-update 3-win 3-scroll))))))))

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
  (unless (vdiff-session-diff-stale vdiff--session)
    (setf (vdiff-session-diff-stale vdiff--session) t)
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
      (vdiff--set-open-fold-props ovr)
      (dolist (other-fold (overlay-get ovr 'vdiff-other-folds))
        (vdiff--set-open-fold-props other-fold)))))

(defun vdiff-close-fold (beg end)
  "Close folds between BEG and END, as well as corresponding ones
in other vdiff buffer. If called interactively, either close fold
at point or on prior line. If the region is active close all
folds in the region."
  (interactive (vdiff--region-or-close-overlay))
  (dolist (ovr (overlays-in beg end))
    (when (eq (overlay-get ovr 'vdiff-type) 'fold)
      (setf (vdiff-session-all-folds-open vdiff--session) nil)
      (goto-char (overlay-start ovr))
      (vdiff--set-closed-fold-props ovr)
      (dolist (other-fold (overlay-get ovr 'vdiff-other-folds))
        (vdiff--set-closed-fold-props other-fold)))))

(defun vdiff-open-all-folds ()
  "Open all folds in both buffers"
  (interactive)
  (save-excursion
    (setf (vdiff-session-all-folds-open vdiff--session) t)
    (vdiff-open-fold (point-min) (point-max))))

(defun vdiff-close-all-folds ()
  "Close all folds in both buffers"
  (interactive)
  (save-excursion
    (setf (vdiff-session-all-folds-open vdiff--session) nil)
    (vdiff-close-fold (point-min) (point-max))))

(defun vdiff-close-other-folds ()
  "Close all other folds in both buffers"
  (interactive)
  (dolist (ovr (overlays-in (point-min) (point-max)))
    (when (and (eq (overlay-get ovr 'vdiff-type) 'fold)
               (not (vdiff--point-in-fold-p ovr)))
      (setf (vdiff-session-all-folds-open vdiff--session) nil)
      (vdiff--set-closed-fold-props ovr)
      (dolist (other-fold (overlay-get ovr 'vdiff-other-folds))
        (vdiff--set-closed-fold-props other-fold)))))

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

;; * Session

;; modified from ediff
(defun vdiff--unique-buffer-name (name)
  (if (null (get-buffer name))
      name
    (let ((n 2))
      (while (get-buffer (format "%s<%d>" name n))
        (setq n (1+ n)))
      (format "%s<%d>" name n))))

(defun vdiff--init-session (buffer-a buffer-b &optional buffer-c)
  (make-vdiff-session
   :buffers (vdiff--non-nil-list buffer-a buffer-b buffer-c)
   :process-buffer (vdiff--unique-buffer-name " *vdiff* ")
   :word-diff-output-buffer (vdiff--unique-buffer-name " *vdiff-word* ")
   :case-args ""
   :whitespace-args ""))

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
    (setq vdiff--temp-session
          (vdiff--init-session buffer-a buffer-b))
    (dolist (buf (list buffer-a buffer-b))
      (with-current-buffer buf
        (vdiff-mode 1))))
  (vdiff-refresh))

(defcustom vdiff-3way-layout-function 'vdiff-3way-layout-function-default
  "Function to layout windows in 3way diffs"
  :group 'vdiff
  :type 'function)

(defun vdiff-3way-layout-function-default (buffer-a buffer-b buffer-c)
  (delete-other-windows)
  (switch-to-buffer buffer-a)
  (goto-char (point-min))
  (set-window-buffer (split-window-vertically) buffer-c)
  (set-window-buffer (split-window-horizontally) buffer-b))

;;;###autoload
(defun vdiff-buffers3 (buffer-a buffer-b buffer-c)
  "Start a vdiff session. If called interactively, you will be
asked to select two buffers."
  (interactive
   (let* ((buffer-a
           (get-buffer
            (read-buffer
             "Buffer 1: " (current-buffer))))
          (buffer-b
           (get-buffer
            (read-buffer
             (format "[2:%s] Buffer 3: " buffer-a)
             (window-buffer (next-window (selected-window))))))
          (buffer-c
           (get-buffer
            (read-buffer
             (format "[1:%s 2:%s] Buffer 3: " buffer-a buffer-b)
             (window-buffer (next-window (selected-window)))))))
     (list buffer-a buffer-b buffer-c)))
  (funcall vdiff-3way-layout-function buffer-a buffer-b buffer-c)
  (setq vdiff--temp-session
        (vdiff--init-session buffer-a buffer-b buffer-c))
  (dolist (buf (list buffer-a buffer-b buffer-c))
    (with-current-buffer buf
      (vdiff-3way-mode 1)))
  (vdiff-refresh))

;;;###autoload
(defun vdiff-files3 (file-a file-b file-c)
  "Start a vdiff session with 3 files. If called interactively,
you will be asked to select two files."
  (interactive
   (let* ((file-a (read-file-name "File 1: "))
          (default-directory
            (file-name-directory file-a))
          (file-b
           (read-file-name
            (format "[1:%s] File 2: "
                    (file-name-nondirectory file-a))))
          (file-c
           (read-file-name
            (format "[1:%s 2:%s] File 3: "
                    (file-name-nondirectory file-a)
                    (file-name-nondirectory file-b)))))
     (list file-a file-b file-c)))
  (vdiff-buffers3 (find-file-noselect file-a)
                  (find-file-noselect file-b)
                  (find-file-noselect file-c)))

(defvar vdiff-quit-hook nil)

(defun vdiff-quit ()
  "Quit `vdiff-mode' and clean up."
  (interactive)
  (dolist (buf (vdiff-session-buffers vdiff--session))
    (with-current-buffer buf
      (vdiff-mode -1)))
  (run-hooks 'vdiff-quit-hook)
  (message "vdiff exited"))

(defvar vdiff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-l"  'vdiff-sync-and-center)
    map))

(defvar vdiff-3way-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map vdiff-mode-map)
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

(defun vdiff--init ()
  ;; this is a buffer-local var
  (setq vdiff--session vdiff--temp-session)
  (setq cursor-in-non-selected-windows nil)
  (add-hook 'after-save-hook #'vdiff-refresh nil t)
  (add-hook 'after-change-functions #'vdiff--after-change-function nil t)
  (add-hook 'pre-command-hook #'vdiff--flag-new-command nil t)
  (setf (vdiff-session-window-config vdiff--session)
        (current-window-configuration)))

(defun vdiff--cleanup ()
  (vdiff--remove-all-overlays)
  (setq cursor-in-non-selected-windows t)
  (remove-hook 'after-save-hook #'vdiff-refresh t)
  (remove-hook 'after-change-functions #'vdiff--after-change-function t)
  (remove-hook 'pre-command-hook #'vdiff--flag-new-command t)
  (when vdiff-scroll-lock-mode
    (vdiff-scroll-lock-mode -1))
  (dolist (buf (list (vdiff-session-process-buffer
                      vdiff--session)
                     (vdiff-session-word-diff-output-buffer
                      vdiff--session)))
    (when (process-live-p (get-buffer-process buf))
      (kill-process (get-buffer-process buf)))
    (when (buffer-live-p buf) (kill-buffer buf)))
  (setq vdiff--session nil))

(define-minor-mode vdiff-mode
  "Minor mode active in a vdiff session involving two
buffers. This sets up key bindings in `vdiff-mode-map' and adds
hooks to refresh diff on changes. This will be enabled
automatically after calling commands like `vdiff-files' or
`vdiff-buffers'."
  nil " vdiff" 'vdiff-mode-map
  (cond (vdiff-mode
         (vdiff--init)
         (when vdiff-lock-scrolling
           (vdiff-scroll-lock-mode 1)))
        (t
         (vdiff--cleanup))))

(define-minor-mode vdiff-3way-mode
  "Minor mode active in a vdiff session involving three
buffers. This sets up key bindings in `vdiff-3way-mode-map' and
adds hooks to refresh diff on changes. This will be enabled
automatically after calling commands like `vdiff-files3' or
`vdiff-buffers3'."
  nil " vdiff3" 'vdiff-3way-mode-map
  (cond (vdiff-3way-mode
         (vdiff--init)
         (when vdiff-lock-scrolling
           (vdiff-scroll-lock-mode 1)))
        (t
         (vdiff--cleanup))))

(define-minor-mode vdiff-scroll-lock-mode
  "Lock scrolling between vdiff buffers. This minor mode will be
enabled automatically if `vdiff-lock-scrolling' is non-nil."
  nil nil nil
  (cond (vdiff-scroll-lock-mode
         (unless (or vdiff-mode vdiff-3way-mode)
           (user-error "Must enable vdiff-mode first"))
         (vdiff--with-all-buffers
          (add-hook 'window-scroll-functions #'vdiff--scroll-function nil t))
         (message "Scrolling locked"))
        (t
         (vdiff--with-all-buffers
          (remove-hook 'window-scroll-functions #'vdiff--scroll-function t))
         (message "Scrolling unlocked"))))

(defun vdiff--current-case ()
  (if (string= "" (vdiff-session-case-args vdiff--session))
      "off"
    "on (-i)"))

(defun vdiff--current-whitespace ()
  (pcase (vdiff-session-whitespace-args vdiff--session)
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
