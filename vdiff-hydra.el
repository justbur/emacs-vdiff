;;; vdiff-hydra.el --- A hydra for vdiff -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/justbur/emacs-vdiff
;; Version: 0
;; Keywords:
;; Package-Requires: ((emacs "24.3") (hydra "0.13.6")

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

;; Defines a hydra for quick access to vdiff commands

;;; Code:
(require 'hydra)
(require 'vdiff)

(defhydra vdiff-hydra (nil nil :hint nil)
  "
 Navigation^^             Transmit^^        Folds^^^^                Other^^
 -^^-------------------  --^^------------  -^^^^------------------  --^-^---------------
 [_n_] next change        [_s_] send        [_o_/_O_] open (all)     [_w_] save buffers
 [_p_] previous change    [_r_] receive     [_c_/_C_] close (all)    [_q_] quit hydra
 [_g_] goto corr. line     ^ ^               ^ ^ ^ ^                 [_Q_] quit vdiff "
  ("g" vdiff-goto-corresponding-line)
  ("n" vdiff-next-change)
  ("p" vdiff-previous-change)
  ("s" vdiff-send-changes)
  ("r" vdiff-receive-changes)
  ("Q" vdiff-quit)
  ("w" vdiff-save-buffers)
  ("o" vdiff-open-fold)
  ("O" vdiff-open-all-folds)
  ("c" vdiff-close-fold)
  ("C" vdiff-close-all-folds)
  ("q" nil :exit t))

(provide 'vdiff-hydra)
;;; vdiff-hydra.el ends here
