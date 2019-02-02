;;; ert-bdd.el --- BDD macros for `ert'. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jason Duncan, all rights reserved

;; Author:  Jason Duncan <jasond496@msn.com>
;; Version: 0.0 alpha1
;; URL: https://github.com/functionreturnfunction/ert-bdd
;; Package-Requires:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; BDD macros for `ert'.

;;; Code:

(require 'ert)

(defvar ert-bdd-description-stack nil)

(defun ert-bdd-make-fn-desc (test)
  (string-join (reverse (cons test ert-bdd-description-stack)) " "))

(defun ert-bdd-make-fn-name (desc)
  (intern
   (replace-regexp-in-string (regexp-quote " ") "-" desc)))

(defmacro describe (description &rest body)
  (declare (indent 1))
  (setq ert-bdd-description-stack
        (cons description ert-bdd-description-stack))
  `(progn
     ,@body
     (setq ert-bdd-description-stack
           (cdr ert-bdd-description-stack))))

(defmacro it (description &rest body)
  (declare (indent 1))
  (let ((desc (ert-bdd-make-fn-desc description)))
    `(ert-deftest ,(ert-bdd-make-fn-name desc) ()
       ,desc ,@body)))

(provide 'ert-bdd)
;;; ert-bdd.el ends here
