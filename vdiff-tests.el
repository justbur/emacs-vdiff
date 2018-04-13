;;; vdiff-tests.el --- tests for vdiff.el -*- lexical-binding: t; -*-

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

(ert-deftest vdiff-test-parsing ()
  ""
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
