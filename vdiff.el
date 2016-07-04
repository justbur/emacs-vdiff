;;; vdiff.el --- Like vimdif for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/justbur/emacs-vdiff
;; Version: 0
;; Keywords:
;; Package-Requires: ((emacs "24.3"))

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

;; ** Introduction

;; vdiff is a diff tool for Emacs that is made to behave like vimdiff, meaning diff
;; information is displayed in buffers as you edit them. There are commands for
;; cycling through the changes detected by =diff= and applying changes from one
;; buffer to the other. 

;; ediff is a powerful diff tool built into Emacs, but it works differently. In
;; ediff you control the diffed buffers through a third control buffer, which works
;; great until you want to edit the buffers directly. I prefer the way vimdiff
;; works, but I am also not necessarily interested in perfectly emulating
;; vimdiff. vdiff does not assume you use evil-mode, but is compatible with it.

;; vdiff is a work in progress, so use it at your own risk. Contributions are very
;; welcome.

;; ** Installation and Usage

;; It will be on MELPA eventually. For now, you have to clone this repository and
;; modify =load-path=. Here's an example =use-package= declaration.

;; (use-package vdiff
;;   :load-path "path/to/vdiff"
;;   :commands (vdiff-buffers vdiff-files)
;;   :config
;;   (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map))

;; The last line puts the main vdiff commands under the =C-c= prefix. With this
;; declaration the key bindings in vdiff buffers are

;; | Key     | Command                         | Description                                        |
;; |---------+---------------------------------+----------------------------------------------------|
;; | =C-c n= | =vdiff-next-change=             | Move to next change in buffer                      |
;; | =C-c p= | =vdiff-previous-change=         | Move to previous change in buffer                  |
;; | =C-c g= | =vdiff-goto-corresponding-line= | Jump to the corresponding line in the other buffer |
;; | =C-c s= | =vdiff-send-changes=            | Send this hunk (or all in region) to other buffer  |
;; | =C-c r= | =vdiff-receive-changes=         | Receive the corresponding hunk from other buffer   |
;; | =C-c w= | =vdiff-save-buffers=            | Save both buffers                                  |
;; | =C-l=   | =vdiff-sync-and-center=         | Recenter both buffers at current line              |

;; ** Further customization
   
;; The current customization options and there defaults are
   
;;   ;; Whether to lock scrolling by default when starting vdiff
;;   (setq vdiff-lock-scrolling t)

;;   ;; external diff program/command to use
;;   (setq vdiff-diff-program "diff")

;;   ;; Extra arguments to pass to diff. If this is set wrong, you may
;;   ;; break vdiff.
;;   (setq vdiff-diff-program-args "")

;;   ;; Commands that should be executed in other vdiff buffer to keep lines in
;;   ;; sync. There is no need to include commands that scroll the buffer here,
;;   ;; because those are handled differently.
;;   (setq vdiff-mirrored-commands '(next-line
;;                                   previous-line
;;                                   evil-next-line
;;                                   evil-previous-line
;;                                   beginning-of-buffer
;;                                   end-of-buffer))
;; 

;;; Code:

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

(defcustom vdiff-mirrored-commands '(next-line
                                     previous-line
                                     evil-next-line
                                     evil-previous-line
                                     beginning-of-buffer
                                     end-of-buffer)
  "Commands that should be executed in other vdiff buffer to keep
lines in sync. There is no need to include commands that scroll
the buffer here, because those are handled differently."
  :group 'vdiff
  :type '(repeat symbol))

(defcustom vdiff-fold-padding 2
  "Unchanged lines to leave unfolded around a fold"
  :group 'vdiff
  :type 'integer)

(defcustom vdiff-fold-format-string "+ %s --- %s lines "
  "Format string for text on closed folds. First element is the
code on the first line being covered. The second is the number of
lines hidden."
  :group 'vdiff
  :type 'string)

(defface vdiff-addition-face
  '((t :background "#27321C"))
  "Face for additions"
  :group 'vdiff)

(defface vdiff-change-face
  '((t :background "#4C3A25"))
  "Face for changes"
  :group 'vdiff)

(defface vdiff-closed-fold-face
  '((t :inherit region))
  "Face for closed folds"
  :group 'vdiff)

(defface vdiff-open-fold-face
  '((t :background "#282828"))
  "Face for open folds"
  :group 'vdiff)

(defface vdiff-subtraction-face
  '((t :background "#3F1B1B"))
  "Face for changes"
  :group 'vdiff)

(defvar vdiff--buffers nil)
(defvar vdiff--temp-files nil)
(defvar vdiff--process-buffer " *vdiff*")
(defvar vdiff--diff-data nil)
(defvar vdiff--diff-code-regexp
  "^\\([0-9]+\\),?\\([0-9]+\\)?\\([adc]\\)\\([0-9]+\\),?\\([0-9]+\\)?")
(defvar vdiff--inhibit-window-switch nil)
(defvar vdiff--scroll-command-cnt 0)
(defvar vdiff--inhibit-sync nil)
(defvar vdiff--line-map nil)

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

(defun vdiff--change-at-point-p ()
  (let ((ovr (vdiff--overlay-at-pos)))
    (and (overlayp ovr)
         (overlay-get ovr 'vdiff-type)
         (not (eq (overlay-get ovr 'vdiff-type) 'fold)))))

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

(defmacro vdiff--with-both-buffers (&rest body)
  `(when (and (buffer-live-p (car vdiff--buffers))
              (buffer-live-p (cadr vdiff--buffers)))
     (dolist (buf vdiff--buffers)
       (with-current-buffer buf
         ,@body))))

(defun vdiff-refresh ()
  "Asynchronously refresh diff information."
  (interactive)
  (let* ((cmd (mapconcat #'identity
                         (list
                          vdiff-diff-program
                          vdiff-diff-program-args
                          (car vdiff--temp-files)
                          (cadr vdiff--temp-files))
                         " "))
         (proc (get-buffer-process
                vdiff--process-buffer)))
    (with-current-buffer (car vdiff--buffers)
      (write-region nil nil (car vdiff--temp-files)))
    (with-current-buffer (cadr vdiff--buffers)
      (write-region nil nil (cadr vdiff--temp-files)))
    (when proc
      (kill-process proc))
    (with-current-buffer (get-buffer-create vdiff--process-buffer)
      (erase-buffer))
    (setq proc (start-process-shell-command
                vdiff--process-buffer
                vdiff--process-buffer
                cmd))
    (set-process-sentinel proc #'vdiff--diff-refresh-1)))

(defun vdiff--diff-refresh-1 (_proc event)
  (cond ((string= "finished\n" event)
         ;; means no difference between files
         (setq vdiff--diff-data nil)
         (vdiff--refresh-overlays))
        ((string= "exited abnormally with code 1\n" event)
         (setq vdiff--diff-data nil)
         (let (res)
           (with-current-buffer vdiff--process-buffer
             (goto-char (point-min))
             (while (re-search-forward vdiff--diff-code-regexp nil t)
               (let ((code (match-string 3))
                     (a-range (cons (vdiff--maybe-int (match-string 1))
                                    (vdiff--maybe-int (match-string 2))))
                     (b-range (cons (vdiff--maybe-int (match-string 4))
                                    (vdiff--maybe-int (match-string 5)))))
                 (push (list code a-range b-range) res))))
           (setq vdiff--diff-data (nreverse res)))
         (vdiff--refresh-overlays))
        ((string-match-p "exited abnormally with code" event)
         (setq vdiff--diff-data nil)
         (vdiff--refresh-overlays)
         (message "vdiff process error: %s" event))))

(defun vdiff--remove-all-overlays ()
  (vdiff--with-both-buffers (remove-overlays)))

(defun vdiff-save-buffers ()
  "Save all vdiff buffers."
  (interactive)
  (vdiff--with-both-buffers (save-buffer)))

;; * Add overlays

(defun vdiff--make-subtraction-string (length)
  (let (string)
    (dotimes (_ length)
      (push (make-string (1- (vdiff--min-window-width)) ?-) string))
    (propertize
     (concat (mapconcat #'identity string "\n") "\n")
     'face 'vdiff-subtraction-face)))

(defun vdiff--add-subtraction-overlays (buffer start-line target-range amount)
  (with-current-buffer buffer
    (vdiff--move-to-line start-line)
    (end-of-line)
    (let* ((position (1+ (point)))
           (ovr (make-overlay position (1+ position))))
      (overlay-put ovr 'before-string 
                   (vdiff--make-subtraction-string amount))
      (overlay-put ovr 'vdiff-target-range target-range)
      (overlay-put ovr 'vdiff-type 'subtraction))))

(defun vdiff--add-change-overlays
    (buffer start-line lines target-range
            &optional addition subtraction-padding)
  (with-current-buffer buffer
    (vdiff--move-to-line start-line)
    (let ((beg (point))
          (end (progn (forward-line lines)
                      (point))))
      (let ((ovr (make-overlay beg end)))
        (overlay-put ovr 'face (if addition
                                   'vdiff-addition-face
                                 'vdiff-change-face))
        (overlay-put ovr 'vdiff-type (if addition
                                         'addition
                                       'change))
        (overlay-put ovr 'vdiff-target-range target-range)
        (when subtraction-padding
          (overlay-put ovr 'after-string
                       (vdiff--make-subtraction-string subtraction-padding)))))))

(defun vdiff--make-fold (buffer range)
  (with-current-buffer buffer
    (let* ((beg-line (+ vdiff-fold-padding (car range)))
           (end-line (1+ (- (cdr range) vdiff-fold-padding)))
           (fold-start (vdiff--pos-at-line-beginning beg-line))
           (summ-text (buffer-substring-no-properties
                       fold-start
                       (min (save-excursion
                              (goto-char fold-start)
                              (line-end-position))
                            (+ fold-start
                               (- (vdiff--min-window-width) 20)))))
           (fold-end
            (vdiff--pos-at-line-beginning end-line))
           (ovr (make-overlay fold-start fold-end))
           (text (format vdiff-fold-format-string
                         summ-text
                         (1+ (- end-line beg-line))))
           (text
            (propertize
             (concat text
                     (make-string (- (vdiff--min-window-width)
                                     (length text) 1) ?-)
                     "\n")
             'face 'vdiff-closed-fold-face)))
      (overlay-put ovr 'face 'vdiff-open-fold-face)
      (overlay-put ovr 'vdiff-fold-text text)
      (overlay-put ovr 'vdiff-type 'fold)
      ovr)))

(defun vdiff--add-folds (a-buffer b-buffer a-range b-range)
  (when (and (> (1+ (- (cdr a-range) (car a-range)))
                (* 2 vdiff-fold-padding))
             (> (- (cdr a-range) (car a-range)) 2))
    (let ((a-fold (vdiff--make-fold a-buffer a-range))
          (b-fold (vdiff--make-fold b-buffer b-range)))
      (overlay-put a-fold 'display (overlay-get a-fold 'vdiff-fold-text))
      (overlay-put a-fold 'vdiff-fold-open nil)
      (overlay-put a-fold 'vdiff-other-fold b-fold)
      (overlay-put b-fold 'display (overlay-get b-fold 'vdiff-fold-text))
      (overlay-put b-fold 'vdiff-fold-open nil)
      (overlay-put b-fold 'vdiff-other-fold a-fold))))

(defun vdiff--refresh-overlays ()
  (vdiff--remove-all-overlays)
  (vdiff--refresh-line-maps)
  (save-excursion
    (let ((a-buffer (car vdiff--buffers))
          (b-buffer (cadr vdiff--buffers))
          (a-last-post-end 1)
          (b-last-post-end 1))
      (dolist (header vdiff--diff-data)
        (let* ((code (nth 0 header))
               (a-range (nth 1 header))
               (b-range (nth 2 header))
               (a-beg (car a-range))
               (a-end (if (cdr-safe a-range)
                          (cdr a-range)
                        (car a-range)))
               (a-norm-range (cons a-beg a-end))
               (a-length (1+ (- a-end a-beg)))
               (b-beg (car b-range))
               (b-end (if (cdr-safe b-range)
                          (cdr b-range)
                        (car b-range)))
               (b-norm-range (cons b-beg b-end))
               (b-length (1+ (- b-end b-beg))))

          (vdiff--add-folds
           a-buffer b-buffer
           (cons a-last-post-end (1- a-beg))
           (cons b-last-post-end (1- b-beg)))
          (setq a-last-post-end (1+ a-end))
          (setq b-last-post-end (1+ b-end))

          (cond ((string= code "d")
                 (vdiff--add-subtraction-overlays
                  b-buffer b-beg a-norm-range a-length)
                 (vdiff--add-change-overlays
                  a-buffer a-beg a-length b-norm-range t))

                ((string= code "a")
                 (vdiff--add-subtraction-overlays
                  a-buffer a-beg b-norm-range b-length)
                 (vdiff--add-change-overlays
                  b-buffer b-beg b-length a-norm-range t))

                ((and (string= code "c") (> a-length b-length))
                 (vdiff--add-change-overlays
                  a-buffer a-beg a-length b-norm-range)
                 (vdiff--add-change-overlays
                  b-buffer b-beg b-length a-norm-range
                  nil (- a-length b-length)))

                ((and (string= code "c") (< a-length b-length))
                 (vdiff--add-change-overlays
                  a-buffer a-beg a-length b-norm-range
                  nil (- b-length a-length))
                 (vdiff--add-change-overlays
                  b-buffer b-beg b-length a-norm-range))

                ((string= code "c")
                 (vdiff--add-change-overlays
                  a-buffer a-beg a-length b-norm-range)
                 (vdiff--add-change-overlays
                  b-buffer b-beg b-length a-norm-range)))))
      (vdiff--add-folds
       a-buffer b-buffer
       (cons a-last-post-end
             (with-current-buffer a-buffer
               (line-number-at-pos (point-max))))
       (cons b-last-post-end
             (with-current-buffer b-buffer
               (line-number-at-pos (point-max))))))))

;; * Moving changes

(defun vdiff--region-or-close-overlay ()
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
  "Send these changes to other vdiff buffer. If the region is
active, send all changes found in the region. Otherwise use the
changes under point or on the immediately preceding line."
  (interactive
   (vdiff--region-or-close-overlay))
  (let* ((ovrs (overlays-in beg end)))
    (dolist (ovr ovrs)
      (cond ((memq (overlay-get ovr 'vdiff-type)
                   '(change addition))
             (vdiff--transmit-change-overlay ovr receive))
            ((eq (overlay-get ovr 'vdiff-type) 'subtraction)
             (vdiff--transmit-subtraction-overlay ovr receive))))
    (vdiff-refresh)))

(defun vdiff-receive-changes (beg end)
  "Receive the changes corresponding to this position from the
other vdiff buffer. If the region is active, receive all
corresponding changes found in the region. Otherwise use the
changes under point or on the immediately preceding line."
  (interactive (vdiff--region-or-close-overlay))
  (vdiff-send-changes beg end t))

(defun vdiff--transmit-change-overlay (chg-ovr &optional receive)
  (cond ((not (overlayp chg-ovr))
         (message "No change found"))
        (receive
         (let* ((target-rng (overlay-get chg-ovr 'vdiff-target-range))
                (pos (vdiff--pos-at-line-beginning
                      (car target-rng) (vdiff--other-buffer))))
           (vdiff--with-other-window
            (vdiff-send-changes pos (1+ pos)))))
        (t
         (let* ((addition (eq 'addition (overlay-get chg-ovr 'vdiff-type)))
                (target-rng (overlay-get chg-ovr 'vdiff-target-range))
                (text (buffer-substring-no-properties
                       (overlay-start chg-ovr)
                       (overlay-end chg-ovr))))
           (with-current-buffer (vdiff--other-buffer)
             (if addition
                 (vdiff--move-to-line (1+ (car target-rng)))
               (vdiff--move-to-line (car target-rng))
               (delete-region (point)
                              (save-excursion
                                (forward-line
                                 (1+ (- (cdr target-rng)
                                        (car target-rng))))
                                (point))))
             (insert text))))))

(defun vdiff--transmit-subtraction-overlay (sub-ovr &optional receive)
  (cond ((not (overlayp sub-ovr))
         (message "No change found"))
        (receive
         (let* ((target-rng (overlay-get sub-ovr 'vdiff-target-range))
                (pos (vdiff--pos-at-line-beginning
                      (car target-rng) (vdiff--other-buffer))))
           (vdiff--with-other-window
            (vdiff-send-changes pos (1+ pos)))))
        (t
         (let* ((target-rng
                 (overlay-get sub-ovr 'vdiff-target-range)))
           (when target-rng
             (with-current-buffer (vdiff--other-buffer)
               (vdiff--move-to-line (car target-rng))
               (delete-region (point)
                              (save-excursion
                                (vdiff--move-to-line
                                 (1+ (cdr target-rng)))
                                (point)))))))))

;; * Scrolling and line syncing

(defun vdiff--refresh-line-maps ()
  (let (new-map)
    (dolist (entry vdiff--diff-data)
      (let* ((code (car entry))
             (a-lines (nth 1 entry))
             (a-beg (car a-lines))
             (a-end (or (cdr-safe a-lines)
                        (car a-lines)))
             (b-lines (nth 2 entry))
             (b-beg (car b-lines))
             (b-end (or (cdr-safe b-lines)
                        (car b-lines))))
        (cond ((string= code "d")
               (push (cons (1- a-beg) b-beg) new-map)
               (push (cons (1+ a-end) (1+ b-end)) new-map))
              ((string= code "a")
               (push (cons a-beg (1- b-beg)) new-map)
               (push (cons (1+ a-end) (1+ b-end)) new-map))
              ((and (string= code "c"))
               (push (cons (1- a-beg) (1- b-beg)) new-map)
               (push (cons (1+ a-end) (1+ b-end)) new-map)))))
    (setq vdiff--line-map (nreverse new-map))))

(defun vdiff--translate-line (line &optional B-to-A)
  (let ((nearest-line
         (catch 'closest
           (let (closest)
             (dolist (entry vdiff--line-map)
               (if (> (if B-to-A
                          (cdr entry)
                        (car entry)) line)
                   (throw 'closest closest)
                 (setq closest entry)))
             (throw 'closest closest)))))
    (cond ((and nearest-line B-to-A)
           (+ (- line (cdr nearest-line)) (car nearest-line)))
          (nearest-line
           (+ (- line (car nearest-line)) (cdr nearest-line)))
          (t line))))

(defun vdiff-goto-corresponding-line (line in-b)
  "Jump to the line in the other vdiff buffer that corresponds to
the current one."
  (interactive (list (line-number-at-pos)
                     (not (vdiff--buffer-a-p))))
  (vdiff-refresh)
  (let* ((new-line (vdiff--translate-line line in-b))
         (new-pos (vdiff--pos-at-line-beginning new-line)))
    (select-window (vdiff--other-window))
    (goto-char new-pos)))

(defun vdiff--sync-line (line in-a)
  "Sync point in the other vdiff buffer to the line in this
buffer. This is usually not necessary."
  (interactive (list (line-number-at-pos)
                     (not (vdiff--buffer-a-p))))
  (let ((new-line (vdiff--translate-line
                   line (not in-a)))
        (other-buffer (vdiff--other-buffer))
        (other-window (vdiff--other-window)))
    (set-window-point
     other-window
     (vdiff--pos-at-line-beginning new-line other-buffer))))

(defun vdiff-sync-and-center ()
  "Sync point in the other vdiff buffer to the line in this
buffer and center both buffers at this line."
  (interactive)
  (vdiff--sync-line (line-number-at-pos) (vdiff--buffer-a-p))
  (recenter)
  (vdiff--with-other-window
   (recenter)))

(defun vdiff--pos-at-line-beginning (line &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (vdiff--move-to-line line)
      (line-beginning-position))))

(defun vdiff-sync-scroll (window window-start)
  "Sync scrolling of all vdiff windows."
  (let* ((buf-a (car vdiff--buffers))
         (buf-b (cadr vdiff--buffers))
         (win-a (get-buffer-window buf-a))
         (win-b (get-buffer-window buf-b)))
    (when (and (eq window (selected-window))
               (window-live-p win-a)
               (window-live-p win-b)
               (memq window (list win-a win-b)))
      (let* ((in-b (eq window win-b))
             (other-window (if in-b win-a win-b))
             (other-buffer (if in-b buf-a buf-b))
             (this-line (line-number-at-pos (point)))
             (other-line (vdiff--translate-line
                          this-line in-b))
             (this-start  (line-number-at-pos window-start))
             (other-start (vdiff--translate-line
                           this-start in-b))
             (other-start-pos (vdiff--pos-at-line-beginning
                               other-start other-buffer)))
        (set-window-start other-window other-start-pos)
        (vdiff--with-other-window
         (goto-char (vdiff--pos-at-line-beginning other-line)))))))

(defun vdiff-mirror-commands ()
  "Execute `vdiff-mirrored-commands' in all buffers."
  ;; Use real-this-command because evil-next-line and evil-previous-line pretend
  ;; they are next-line and previous-line
  (when (and (memq real-this-command vdiff-mirrored-commands)
             (not vdiff--inhibit-sync))
    (let ((this-line (line-number-at-pos))
          ;; This is necessary to not screw up the cursor column after calling
          ;; next-line or previous-line again from the other buffer
          temporary-goal-column)
      (vdiff--with-other-window
       (ignore-errors
         (let ((vdiff--inhibit-sync t))
           (call-interactively real-this-command))
         (if (< vdiff--scroll-command-cnt 40)
             (incf vdiff--scroll-command-cnt)
           ;; (message "syncing lines")
           (setq vdiff--scroll-command-cnt 0)
           (vdiff--move-to-line
            (vdiff--translate-line
             this-line (vdiff--buffer-a-p)))))))))

(defun vdiff-open-fold (beg end)
  "Open folds between BEG and END, as well as corresponding ones
in other vdiff buffer. If called interactively, either open fold
at point or on prior line. If the region is active open all folds
in the region."
  (interactive (vdiff--region-or-close-overlay))
  (dolist (ovr (overlays-in beg end))
    (when (eq (overlay-get ovr 'vdiff-type) 'fold)
      (let ((other-fold (overlay-get ovr 'vdiff-other-fold)))
        (dolist (ovr1 (list ovr other-fold))
          (overlay-put ovr1 'vdiff-fold-open t)
          (overlay-put ovr1 'display nil)
          (overlay-put ovr1 'line-prefix
                       (propertize " "
                        'display '(left-fringe vertical-bar))))))))

(defun vdiff-close-fold (beg end)
  "Close folds between BEG and END, as well as corresponding ones
in other vdiff buffer. If called interactively, either close fold
at point or on prior line. If the region is active close all
folds in the region."
  (interactive (vdiff--region-or-close-overlay))
  (dolist (ovr (overlays-in beg end))
    (when (eq (overlay-get ovr 'vdiff-type) 'fold)
      (let ((other-fold (overlay-get ovr 'vdiff-other-fold)))
        (dolist (ovr1 (list ovr other-fold))
          (overlay-put ovr1 'vdiff-fold-open nil)
          (overlay-put ovr1 'line-prefix nil)
          (overlay-put ovr1 'display
                       (overlay-get ovr1 'vdiff-fold-text)))))))

(defun vdiff-open-all-folds ()
  "Open all folds in both buffers"
  (interactive)
  (vdiff-open-fold (point-min) (point-max)))

(defun vdiff-close-all-folds ()
  "Close all folds in both buffers"
  (interactive)
  (vdiff-close-fold (point-min) (point-max)))

;; * Movement

(defun vdiff--nth-change (&optional n)
  (let* ((n (or n 1))
         (reverse (< n 0))
         pnt)
    (save-excursion
      (dotimes (i (abs n))
        ;; Escape current overlay
        (vdiff--maybe-exit-overlay reverse)
        (setq pnt (point))
        ;; Find next overlay
        (while (and (not (or (eobp) (bobp)))
                    (not (vdiff--change-at-point-p)))
          (setq pnt
                (goto-char (if reverse
                               (previous-overlay-change pnt)
                             (next-overlay-change pnt)))))))
    pnt))

(defun vdiff-next-change (arg)
  "Jump to next change in this buffer."
  (interactive "p")
  (let ((count (or arg 1)))
    (goto-char (vdiff--nth-change count))))

(defun vdiff-previous-change (arg)
  "Jump to previous change in this buffer."
  (interactive "p")
  (let ((count (or (- arg) -1)))
    (goto-char (vdiff--nth-change count))))

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
  (let (window-b buffer-a)
    (delete-other-windows)
    (find-file file-a)
    (goto-char (point-min))
    (setq buffer-a (current-buffer))
    (save-selected-window
      (setq window-b (if horizontal
                         (split-window-vertically)
                       (split-window-horizontally)))
      (find-file-other-window file-b)
      (setq vdiff--buffers (list buffer-a (window-buffer window-b)))
      (vdiff-mode 1))
    (vdiff-mode 1)
    (vdiff-refresh)))

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
    (vdiff--with-both-buffers
     (vdiff-mode 1))
    (vdiff-refresh)))

(defvar vdiff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-l"  'vdiff-sync-and-center)
    map))

(defvar vdiff-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'vdiff-goto-corresponding-line)
    (define-key map "n" 'vdiff-next-change)
    (define-key map "p" 'vdiff-previous-change)
    (define-key map "s" 'vdiff-send-changes)
    (define-key map "r" 'vdiff-receive-changes)
    (define-key map "w" 'vdiff-save-buffers)
    (define-key map "o" 'vdiff-open-fold)
    (define-key map "O" 'vdiff-open-all-folds)
    (define-key map "c" 'vdiff-close-fold)
    (define-key map "C" 'vdiff-close-all-folds)
    map))

(define-minor-mode vdiff-mode
  "Minor mode active in a vdiff session. This sets up key
bindings in `vdiff-mode-map' and adds hooks to refresh diff on
changes. This will be enabled automatically after calling
commands like `vdiff-files' or `vdiff-buffers'."
  nil " vdiff" 'vdiff-mode-map
  (cond (vdiff-mode
         (setq vdiff--temp-files
               (list (make-temp-file "vdiff--temp-a-")
                     (make-temp-file "vdiff--temp-b-")))
         (setq cursor-in-non-selected-windows nil)
         (add-hook 'after-save-hook #'vdiff-refresh nil t)
         (when vdiff-lock-scrolling
           (vdiff-scroll-lock-mode 1)))
        (t
         (vdiff--remove-all-overlays)
         (setq cursor-in-non-selected-windows t)
         (remove-hook 'after-save-hook #'vdiff-refresh t)
         (when vdiff-scroll-lock-mode
           (vdiff-scroll-lock-mode -1))
         (setq vdiff--diff-data nil)
         (setq vdiff--buffers nil)
         (setq vdiff--line-map nil)
         (setq vdiff--temp-files nil)
         (when (process-live-p vdiff--process-buffer)
           (kill-process vdiff--process-buffer))
         (when (buffer-live-p vdiff--process-buffer)
           (kill-buffer vdiff--process-buffer))
         (setq vdiff--scroll-command-cnt 0))))

(define-minor-mode vdiff-scroll-lock-mode
  "Lock scrolling between vdiff buffers. This minor mode will be
enabled automatically if `vdiff-lock-scrolling' is non-nil."
  nil nil nil
  (cond (vdiff-scroll-lock-mode
         (unless vdiff-mode
           (vdiff-mode 1))
         (vdiff--with-both-buffers
          (add-hook 'window-scroll-functions #'vdiff-sync-scroll nil t)
          (add-hook 'post-command-hook #'vdiff-mirror-commands nil t))
         (message "Scrolling locked"))
        (t
         (vdiff--with-both-buffers
          (remove-hook 'window-scroll-functions #'vdiff-sync-scroll t)
          (remove-hook 'post-command-hook #'vdiff-mirror-commands t))
         (message "Scrolling unlocked"))))

(provide 'vdiff)
;;; vdiff.el ends here
