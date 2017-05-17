;;; vdiff-magit.el --- magit integration for vdiff -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Justin Burkett <justin@burkett.cc>

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

;; magit integration for vdiff

;;; Code:

(require 'vdiff)
(require 'magit)
(require 'magit-ediff)

(defgroup vdiff-magit nil
  "vdiff support for Magit."
  :group 'magit-extensions)

(defcustom vdiff-magit-dwim-show-on-hunks nil
  "Whether `vdiff-magit-dwim' runs show variants on hunks.
If non-nil, `vdiff-magit-show-staged' or
`vdiff-magit-show-unstaged' are called based on what section the
hunk is in.  Otherwise, `vdiff-magit-dwim' runs
`vdiff-magit-stage' when point is on an uncommitted hunk."
  ;; :package-version '(magit . "2.2.0")
  :group 'vdiff-magit
  :type 'boolean)

(defcustom vdiff-magit-show-stash-with-index t
  "Whether `vdiff-magit-show-stash' shows the state of the index.

If non-nil, use a third vdiff buffer to distinguish which changes
in the stash were staged.  In cases where the stash contains no
staged changes, fall back to a two-buffer vdiff.

More specificaly, a stash is a merge commit, stash@{N}, with
potentially three parents.

* stash@{N}^1 represents the HEAD commit at the time the stash
  was created.

* stash@{N}^2 records any changes that were staged when the stash
  was made.

* stash@{N}^3, if it exists, contains files that were untracked
  when stashing.

If this option is non-nil, `vdiff-magit-show-stash' will run
vdiff on a file using three buffers: one for stash@{N}, another
for stash@{N}^1, and a third for stash@{N}^2.

Otherwise, vdiff uses two buffers, comparing
stash@{N}^1..stash@{N}.  Along with any unstaged changes, changes
in the index commit, stash@{N}^2, will be shown in this
comparison unless they conflicted with changes in the working
tree at the time of stashing."
  ;; :package-version '(magit . "2.6.0")
  :group 'vdiff-magit
  :type 'boolean)

(defcustom vdiff-magit-use-ediff-for-merges nil
  "If non-nil prefer using `magit-ediff-resolve' over `vdiff-magit-resolve'.

The vdiff-magit version only supports 2-way merges right now and
not 3-way ones. If you use `vdiff-magit-resolve' in a situation
requiring a 3-way merge it will abort and forward to
`magit-ediff-resolve' instead. The purpose of this flag is to
make the merge experience consistent across all types of
merges."
  :group 'vdiff-magit
  :type 'boolean)

(defcustom vdiff-magit-stage-is-2way nil
  "If non-nil `vdiff-magit-stage' will only show two buffers, the
file and the index with the HEAD omitted."
  :group 'vdiff-magit
  :type 'boolean)

;; (defvar magit-ediff-previous-winconf nil)

;;;###autoload (autoload 'vdiff-magit-popup "vdiff-magit" nil t)
(magit-define-popup vdiff-magit-popup
  "Popup console for vdiff commands."
  :actions '((?d "Dwim"          vdiff-magit-dwim)
             (?u "Show unstaged" vdiff-magit-show-unstaged)
             (?s "Stage (vdiff)" vdiff-magit-stage)
             (?i "Show staged"   vdiff-magit-show-staged)
             (?m "Resolve"       vdiff-magit-resolve)
             (?w "Show worktree" vdiff-magit-show-working-tree)
             (?r "Diff range"    vdiff-magit-compare)
             (?c "Show commit"   vdiff-magit-show-commit) nil
             (?z "Show stash"    vdiff-magit-show-stash))
  :max-action-columns 2)

;;;###autoload
(defun vdiff-magit-resolve (file)
  "Resolve outstanding conflicts in FILE using vdiff.
FILE has to be relative to the top directory of the repository.

In the rare event that you want to manually resolve all
conflicts, including those already resolved by Git, use
`ediff-merge-revisions-with-ancestor'."
  (interactive
   (let ((current  (magit-current-file))
         (unmerged (magit-unmerged-files)))
     (unless unmerged
       (user-error "There are no unresolved conflicts"))
     (list (magit-completing-read "Resolve file" unmerged nil t nil nil
                                  (car (member current unmerged))))))
  (if vdiff-magit-use-ediff-for-merges
      (magit-ediff-resolve file)
    (vdiff-merge-conflict file)))

;;;###autoload
(defun vdiff-magit-stage (file)
  "Stage and unstage changes to FILE using vdiff.
FILE has to be relative to the top directory of the repository."
  (interactive
   (list (magit-completing-read "Selectively stage file" nil
                                (magit-tracked-files) nil nil nil
                                (magit-current-file))))
  (magit-with-toplevel
    (let* ((buf-a (or (magit-get-revision-buffer "HEAD" file)
                      (magit-find-file-noselect "HEAD" file)))
           (buf-b (with-current-buffer (magit-find-file-index-noselect file t)
                    (setq buffer-read-only nil)
                    (current-buffer)))
           (buf-c (get-file-buffer file))
           (file-buf-c (or buf-c (find-file-noselect file)))
           (coding-system-for-read
            (with-current-buffer file-buf-c buffer-file-coding-system)))
      (if vdiff-magit-stage-is-2way
          (vdiff-buffers
           buf-b file-buf-c nil
           `(lambda (buf-b buf-c)
              (when (and (buffer-live-p buf-b)
                         (buffer-modified-p buf-b))
                (with-current-buffer buf-b
                  (magit-update-index))
                (kill-buffer buf-b))
              (when (and (buffer-live-p buf-c)
                         (buffer-modified-p buf-c))
                (with-current-buffer buf-c
                  (when (y-or-n-p
                         (format "Save file %s? " buffer-file-name))
                    (save-buffer))))
              ;; kill buf-c if it wasn't open originally
              (unless ,buf-c (kill-buffer buf-c)))
           t nil)
        (vdiff-buffers3
         buf-a buf-b file-buf-c
         `(lambda (buf-a buf-b buf-c)
            (when (and (buffer-live-p buf-b)
                       (buffer-modified-p buf-b))
              (with-current-buffer buf-b
                (magit-update-index))
              (kill-buffer buf-b))
            (when (and (buffer-live-p buf-c)
                       (buffer-modified-p buf-c))
              (with-current-buffer buf-c
                (when (y-or-n-p
                       (format "Save file %s? " buffer-file-name))
                  (save-buffer))))
            ;; kill buf-c if it wasn't open originally
            (unless ,buf-c (kill-buffer buf-c))
            (when (buffer-live-p buf-a)
              (kill-buffer buf-a)))
         t nil)))))

;; ;;;###autoload
(defun vdiff-magit-compare (rev-a rev-b file-a file-b)
  "Compare REVA:FILEA with REVB:FILEB using vdiff.

FILEA and FILEB have to be relative to the top directory of the
repository.  If REVA or REVB is nil then this stands for the
working tree state.

If the region is active, use the revisions on the first and last
line of the region.  With a prefix argument, instead of diffing
the revisions, choose a revision to view changes along, starting
at the common ancestor of both revisions (i.e., use a \"...\"
range)."
  (interactive (-let [(rev-a rev-b) (magit-ediff-compare--read-revisions
                                   nil current-prefix-arg)]
                 (nconc (list rev-a rev-b)
                        (magit-ediff-read-files rev-a rev-b))))
  (magit-with-toplevel
    (vdiff-buffers
     (if rev-a
         (or (magit-get-revision-buffer rev-a file-a)
             (magit-find-file-noselect rev-a file-a))
       (or (get-file-buffer file-a)
           (find-file-noselect file-a)))
     (if rev-b
         (or (magit-get-revision-buffer rev-b file-b)
             (magit-find-file-noselect rev-b file-b))
       (or (get-file-buffer file-b)
           (find-file-noselect file-b)))
     nil t t)))

;;;###autoload
(defun vdiff-magit-dwim ()
  "Compare, stage, or resolve using vdiff.

This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using vdiff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `vdiff-magit-*'
command has to be used explicitly.  If it cannot read the user's
mind at all, then it asks the user for a command to run."
  (interactive)
  (magit-section-case
    (hunk (save-excursion
            (goto-char (magit-section-start (magit-section-parent it)))
            (vdiff-magit-dwim)))
    (t
     (let ((range (magit-diff--dwim))
           (file (magit-current-file))
           command rev-a rev-b)
       (pcase range
         ((and (guard (not vdiff-magit-dwim-show-on-hunks))
               (or `unstaged `staged))
          (setq command (if (magit-anything-unmerged-p)
                            #'vdiff-magit-resolve
                          #'vdiff-magit-stage)))
         (`unstaged (setq command #'vdiff-magit-show-unstaged))
         (`staged (setq command #'vdiff-magit-show-staged))
         (`(commit . ,value)
          (setq command #'vdiff-magit-show-commit
                rev-b value))
         (`(stash . ,value)
          (setq command #'vdiff-magit-show-stash
                rev-b value))
         ((pred stringp)
          (-let [(a b) (magit-ediff-compare--read-revisions range)]
            (setq command #'vdiff-magit-compare
                  rev-a a
                  rev-b b)))
         (_
          (when (derived-mode-p 'magit-diff-mode)
            (pcase (magit-diff-type)
              (`committed (-let [(a b) (magit-ediff-compare--read-revisions
                                        (car magit-refresh-args))]
                            (setq rev-a a rev-b b)))
              ((guard (not vdiff-magit-dwim-show-on-hunks))
               (setq command #'vdiff-magit-stage))
              (`unstaged  (setq command #'vdiff-magit-show-unstaged))
              (`staged    (setq command #'vdiff-magit-show-staged))
              (`undefined (setq command nil))
              (_          (setq command nil))))))
       (cond ((not command)
              (call-interactively
               (magit-read-char-case
                   "Failed to read your mind; do you want to " t
                 (?c "[c]ommit"  'vdiff-magit-show-commit)
                 (?r "[r]ange"   'vdiff-magit-compare)
                 (?s "[s]tage"   'vdiff-magit-stage)
                 (?v "resol[v]e" 'vdiff-magit-resolve))))
             ((eq command 'vdiff-magit-compare)
              (apply 'vdiff-magit-compare rev-a rev-b
                     (magit-ediff-read-files rev-a rev-b file)))
             ((eq command 'vdiff-magit-show-commit)
              (vdiff-magit-show-commit rev-b))
             ((eq command 'vdiff-magit-show-stash)
              (vdiff-magit-show-stash rev-b))
             (file
              (funcall command file))
             (t
              (call-interactively command)))))))

;; ;;;###autoload
(defun vdiff-magit-show-staged (file)
  "Show staged changes using vdiff.

This only allows looking at the changes; to stage, unstage,
and discard changes using vdiff, use `vdiff-magit-stage'.

FILE must be relative to the top directory of the repository."
  (interactive
   (list (magit-read-file-choice "Show staged changes for file"
                                 (magit-staged-files)
                                 "No staged files")))
  (vdiff-buffers
   (or (magit-get-revision-buffer "HEAD" file)
       (magit-find-file-noselect "HEAD" file))
   (or (get-buffer (concat file ".~{index}~"))
       (magit-find-file-index-noselect file t))
   nil t t))

;;;###autoload
(defun vdiff-magit-show-unstaged (file)
  "Show unstaged changes using vdiff.

This only allows looking at the changes; to stage, unstage,
and discard changes using vdiff, use `vdiff-magit-stage'.

FILE must be relative to the top directory of the repository."
  (interactive
   (list (magit-read-file-choice "Show unstaged changes for file"
                                 (magit-modified-files)
                                 "No unstaged files")))
  (magit-with-toplevel
    (vdiff-buffers
     (or (get-buffer (concat file ".~{index}~"))
         (magit-find-file-index-noselect file t))
     (or (get-file-buffer file)
         (find-file-noselect file))
     nil t t)))

;; ;;;###autoload
(defun vdiff-magit-show-working-tree (file)
  "Show changes between HEAD and working tree using vdiff.
FILE must be relative to the top directory of the repository."
  (interactive
   (list (magit-read-file-choice "Show changes in file"
                                 (magit-changed-files "HEAD")
                                 "No changed files")))
  (magit-with-toplevel
    (vdiff-buffers
     (or (magit-get-revision-buffer "HEAD" file)
         (magit-find-file-noselect "HEAD" file))
     (or (get-file-buffer file) (find-file-noselect file))
     nil t t)))

;; ;;;###autoload
(defun vdiff-magit-show-commit (commit)
  "Show changes introduced by COMMIT using vdiff."
  (interactive (list (magit-read-branch-or-commit "Revision")))
  (let ((rev-a (concat commit "^"))
        (rev-b commit))
    (apply #'vdiff-magit-compare
           (concat commit "^") commit
           (magit-ediff-read-files rev-a rev-b (magit-current-file)))))

;; ;;;###autoload
(defun vdiff-magit-show-stash (stash)
  "Show changes introduced by STASH using vdiff.
`vdiff-magit-show-stash-with-index' controls whether a
three-buffer vdiff is used in order to distinguish changes in the
stash that were staged."
  (interactive (list (magit-read-stash "Stash")))
  (-let* ((rev-a (concat stash "^1"))
          (rev-b (concat stash "^2"))
          (rev-c stash)
          ((file-a file-c) (magit-ediff-read-files rev-a rev-c))
          (file-b file-c))
    (if (and vdiff-magit-show-stash-with-index
             (member file-a (magit-changed-files rev-b rev-a)))
        (let ((buf-a (magit-get-revision-buffer rev-a file-a))
              (buf-b (magit-get-revision-buffer rev-b file-b))
              (buf-c (magit-get-revision-buffer rev-c file-c)))
          (vdiff-buffers3
           (or buf-a (magit-find-file-noselect rev-a file-a))
           (or buf-b (magit-find-file-noselect rev-b file-b))
           (or buf-c (magit-find-file-noselect rev-c file-c))
           nil t t))
      (vdiff-magit-compare rev-a rev-c file-a file-c))))

(provide 'vdiff-magit)
;;; vdiff-magit.el ends here
