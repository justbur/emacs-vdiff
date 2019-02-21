;;; vdiff-test.el --- tests for vdiff.el -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Free Software Foundation, Inc.

;; Author: Justin Burkett <justin@burkett.cc>
;; Maintainer: Justin Burkett <justin@burkett.cc>

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

;;; Code:

(require 'ert)
(require 'vdiff)

(defmacro vdiff-test-with-buffers
    (a-string b-string operation final-a-string final-b-string &optional after-quit)
  `(let ((buffer-a (get-buffer-create "vdiff-tests-buffer-a"))
         (buffer-b (get-buffer-create "vdiff-tests-buffer-b"))
         (vdiff--testing-mode t))
     (unwind-protect
         (progn
           (with-current-buffer buffer-a
             (erase-buffer)
             (insert ,(replace-regexp-in-string "|" "\n" a-string)))
           (with-current-buffer buffer-b
             (erase-buffer)
             (insert ,(replace-regexp-in-string "|" "\n" b-string)))
           (vdiff-buffers buffer-a buffer-b)
           ,operation
           (with-current-buffer buffer-a
             (should (string= (buffer-string)
                              ,(replace-regexp-in-string "|" "\n" final-a-string))))
           (with-current-buffer buffer-b
             (should (string= (buffer-string)
                              ,(replace-regexp-in-string "|" "\n" final-b-string)))))
       (with-current-buffer buffer-a
         (vdiff-quit))
       ,after-quit
       (kill-buffer buffer-a)
       (kill-buffer buffer-b))))

(ert-deftest vdiff-test-parsing ()
  "Test parsing of unified diff format."
  (with-temp-buffer
    (insert "--- test1.txt	2018-04-13 11:11:41.000000000 -0400
+++ test2.txt	2018-04-13 11:11:46.000000000 -0400
@@ -1,3 +1,6 @@
+
+
+
 1
 2
 3
@@ -9,6 +12,8 @@
 9
 10
 11
+11
+11
 12
 13
 14
@@ -16,7 +21,8 @@
 16
 17
 18
-19
-20
+18
+29
 21
 22
+23
")
    (should (equal (vdiff--parse-diff-u (current-buffer))
                   '(((1) (1 . 3)) ((12) (15 . 16)) ((19 . 20) (24 . 25)) ((23) (28 . 28)))))))

(ert-deftest vdiff-test-setup ()
  "Test setting up `vdiff-mode'."
  ;; Setup does not change buffers
  (vdiff-test-with-buffers
   "1|2|3|4|5|6|7|8|9|10|"
   "1|2|4|4|5|6|8|8|9|10|"
   nil
   "1|2|3|4|5|6|7|8|9|10|"
   "1|2|4|4|5|6|8|8|9|10|"))

(ert-deftest vdiff-test-movement ()
  "Test movement in buffers."
  (vdiff-test-with-buffers
   "1|2|3|4|5|6|7|8|9|10|"
   "1|2|4|4|5|6|8|8|9|10|"
   (with-current-buffer buffer-a
     (goto-char (point-min))
     (call-interactively 'vdiff-next-hunk)
     (call-interactively 'vdiff-next-hunk)
     (should (looking-at-p "7")))
   "1|2|3|4|5|6|7|8|9|10|"
   "1|2|4|4|5|6|8|8|9|10|"))

(ert-deftest vdiff-test-transmiting ()
  "Test transmitting changes."
  ;; Test sending first change
  (vdiff-test-with-buffers
   "1|2|3|4|5|6|7|8|9|10|"
   "1|2|4|4|5|6|8|8|9|10|"
   (with-current-buffer buffer-a
     (goto-char (point-min))
     (call-interactively 'vdiff-next-hunk)
     (call-interactively 'vdiff-send-changes))
   "1|2|3|4|5|6|7|8|9|10|"
   "1|2|3|4|5|6|8|8|9|10|")
  ;; Test sending everything
  (vdiff-test-with-buffers
   "1|2|3|4|5|6|7|8|9|10|"
   "1|2|4|4|5|6|8|8|9|10|"
   (with-current-buffer buffer-a
     (vdiff-send-changes (point-min) (point-max)))
   "1|2|3|4|5|6|7|8|9|10|"
   "1|2|3|4|5|6|7|8|9|10|"))

(ert-deftest vdiff-test-receiving ()
  "Test receiving changes."
  ;; Test receiving first change
  (vdiff-test-with-buffers
   "1|2|3|4|5|6|7|8|9|10|"
   "1|2|4|4|5|6|8|8|9|10|"
   (with-current-buffer buffer-b
     (goto-char (point-min))
     (call-interactively 'vdiff-next-hunk)
     (call-interactively 'vdiff-receive-changes))
   "1|2|3|4|5|6|7|8|9|10|"
   "1|2|3|4|5|6|8|8|9|10|")
  ;; Test receiving everything
  (vdiff-test-with-buffers
   "1|2|3|4|5|6|7|8|9|10|"
   "1|2|4|4|5|6|8|8|9|10|"
   (with-current-buffer buffer-b
     (vdiff-receive-changes (point-min) (point-max)))
   "1|2|3|4|5|6|7|8|9|10|"
   "1|2|3|4|5|6|7|8|9|10|"))


(ert-deftest vdiff-test-selective-transmiting ()
  "Test transmitting changes when region is active."
  ;; Test sending first line of first change
  (vdiff-test-with-buffers
   "1|2|3|4|5|6|7|8|9|10|"
   "1|x|x|x|5|6|8|8|9|10|"
   (with-current-buffer buffer-a
     (goto-char (point-min))
     (forward-line)
     (set-mark (point))
     (forward-line)
     (call-interactively 'vdiff-send-changes))
   "1|2|3|4|5|6|7|8|9|10|"
   "1|2|x|x|5|6|8|8|9|10|")
  ;; Test sending second line of first change
  (vdiff-test-with-buffers
   "1|2|3|4|5|6|7|8|9|10|"
   "1|x|x|x|5|6|8|8|9|10|"
   (with-current-buffer buffer-a
     (goto-char (point-min))
     (forward-line 2)
     (set-mark (point))
     (forward-line)
     (call-interactively 'vdiff-send-changes))
   "1|2|3|4|5|6|7|8|9|10|"
   "1|x|3|x|5|6|8|8|9|10|")
  ;; Test sending first line of first change when region begins before
  (vdiff-test-with-buffers
   "1|2|3|4|5|6|7|8|9|10|"
   "1|x|x|x|5|6|8|8|9|10|"
   (with-current-buffer buffer-a
     (goto-char (point-min))
     (set-mark (point))
     (forward-line 2)
     (call-interactively 'vdiff-send-changes))
   "1|2|3|4|5|6|7|8|9|10|"
   "1|2|x|x|5|6|8|8|9|10|")
  ;; Test sending last two lines of first change when region ends after
  (vdiff-test-with-buffers
   "1|2|3|4|5|6|7|8|9|10|"
   "1|x|x|x|5|6|8|8|9|10|"
   (with-current-buffer buffer-a
     (goto-char (point-min))
     (forward-line 2)
     (set-mark (point))
     (forward-line 3)
     (call-interactively 'vdiff-send-changes))
   "1|2|3|4|5|6|7|8|9|10|"
   "1|x|3|4|5|6|8|8|9|10|"))

(provide 'vdiff-test)
;;; vdiff-test.el ends here
