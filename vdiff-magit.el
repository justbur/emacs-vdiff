;;; vdiff-magit.el --- magit integration for vdiff -*- lexical-binding: t; -*-

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

;; Contributions and suggestions are very welcome.

;; See https://github.com/justbur/emacs-vdiff for more information

;;; Code:

(require 'vdiff)
(require 'magit)

;;;###autoload
(defun vdiff-magit-stage (file)
  "Stage and unstage changes to FILE using vdiff.
FILE has to be relative to the top directory of the repository."
  (interactive
   (list (magit-completing-read "Selectively stage file" nil
                                (magit-tracked-files) nil nil nil
                                (magit-current-file))))
  (magit-with-toplevel
    (let* ((conf (current-window-configuration))
           (bufA (magit-get-revision-buffer "HEAD" file))
           (bufB (get-buffer (concat file ".~{index}~")))
           (bufBrw (and bufB (with-current-buffer bufB (not buffer-read-only))))
           (bufC (get-file-buffer file))
           (fileBufC (or bufC (find-file-noselect file)))
           (coding-system-for-read
            (with-current-buffer fileBufC buffer-file-coding-system)))
      (vdiff-buffers3
       (or bufA (magit-find-file-noselect "HEAD" file))
       (with-current-buffer (magit-find-file-index-noselect file t)
         (setq buffer-read-only nil)
         (current-buffer))
       fileBufC
       (lambda (bufA bufB bufC)
         (and (buffer-live-p bufB)
              (buffer-modified-p bufB)
              (with-current-buffer bufB
                (magit-update-index)))
         (and (buffer-live-p bufC)
              (buffer-modified-p bufC)
              (with-current-buffer bufC
                (when (y-or-n-p
                       (format "Save file %s? " buffer-file-name))
                  (save-buffer))))
         ;; (let ((magit-ediff-previous-winconf ,conf))
         ;;   (run-hooks 'magit-ediff-quit-hook))
         )
       nil t))))

;;;###autoload
(defun vdiff-magit-dwim ()
  "Compare, stage, or resolve using vdiff if possible.

Falls back to ediff commands. This command tries to guess what
file, and what commit or range the user wants to compare, stage,
or resolve using Ediff.  It might only be able to guess either
the file, or range or commit, in which case the user is asked
about the other.  It might not always guess right, in which case
the appropriate `magit-ediff-*' command has to be used
explicitly.  If it cannot read the user's mind at all, then it
asks the user for a command to run."
  (interactive)
  (magit-section-case
    (hunk (save-excursion
            (goto-char (magit-section-start (magit-section-parent it)))
            (magit-ediff-dwim)))
    (t
     (let ((range (magit-diff--dwim))
           (file (magit-current-file))
           command revA revB)
       (pcase range
         ((and (guard (not magit-ediff-dwim-show-on-hunks))
               (or `unstaged `staged))
          (setq command (if (magit-anything-unmerged-p)
                            #'magit-ediff-resolve
                          #'vdiff-magit-stage)))
         (`unstaged (setq command #'magit-ediff-show-unstaged))
         (`staged (setq command #'magit-ediff-show-staged))
         (`(commit . ,value)
          (setq command #'magit-ediff-show-commit
                revB value))
         (`(stash . ,value)
          (setq command #'magit-ediff-show-stash
                revB value))
         ((pred stringp)
          (-let [(a b) (magit-ediff-compare--read-revisions range)]
            (setq command #'magit-ediff-compare
                  revA a
                  revB b)))
         (_
          (when (derived-mode-p 'magit-diff-mode)
            (pcase (magit-diff-type)
              (`committed (-let [(a b) (magit-ediff-compare--read-revisions
                                        (car magit-refresh-args))]
                            (setq revA a revB b)))
              ((guard (not magit-ediff-dwim-show-on-hunks))
               (setq command #'vdiff-magit-stage))
              (`unstaged  (setq command #'magit-ediff-show-unstaged))
              (`staged    (setq command #'magit-ediff-show-staged))
              (`undefined (setq command nil))
              (_          (setq command nil))))))
       (cond ((not command)
              (call-interactively
               (magit-read-char-case
                   "Failed to read your mind; do you want to " t
                 (?c "[c]ommit"  'magit-ediff-show-commit)
                 (?r "[r]ange"   'magit-ediff-compare)
                 (?s "[s]tage"   'vdiff-magit-stage)
                 (?v "resol[v]e" 'magit-ediff-resolve))))
             ((eq command 'magit-ediff-compare)
              (apply 'magit-ediff-compare revA revB
                     (magit-ediff-read-files revA revB file)))
             ((eq command 'magit-ediff-show-commit)
              (magit-ediff-show-commit revB))
             ((eq command 'magit-ediff-show-stash)
              (magit-ediff-show-stash revB))
             (file
              (funcall command file))
             (t
              (call-interactively command)))))))

(provide 'vdiff-magit)
;;; vdiff-magit.el ends here
