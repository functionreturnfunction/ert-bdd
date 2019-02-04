;;; ert-bdd.el --- BDD macros for `ert'. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jason Duncan, all rights reserved

;; Author:  Jason Duncan <jasond496@msn.com>
;; Version: 0.0 alpha1
;; URL: https://github.com/functionreturnfunction/ert-bdd
;; Package-Requires: ((emacs "24"))

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
(defvar ert-bdd-before-stack nil)
(defvar ert-bdd-after-stack nil)

(progn
  (setq ert-bdd-description-stack nil)
  (setq ert-bdd-before-stack nil)
  (setq ert-bdd-after-stack nil))

(if (functionp 'string-join)
    (defalias 'ert-bdd-string-join 'string-join)
  (defun ert-bdd-string-join (list sep)
    (mapconcat #'identity list sep)))

(defun ert-bdd-add-to-setup-stack (stack &rest body)
  (set stack
       (append
        (symbol-value stack)
        `((,(or (car (symbol-value stack)) "global") ,@body))))
  '(ignore))

(defun ert-bdd-remove-from-setup-stack (stack description)
  (set stack
       (remove* description (symbol-value stack) :test 'equal :key 'car)))

(defun ert-bdd-build-description-stack (test sep)
  "Build a description for the current TEST using SEP to join the description stack."
  (ert-bdd-string-join (reverse (cons test ert-bdd-description-stack)) sep))

(defun ert-bdd-build-test-body (test)
  (append (mapcar #'cadr ert-bdd-before-stack)
          test
          (mapcar #'cadr ert-bdd-after-stack)))

(defun ert-bdd-make-fn-desc (test)
  "Build a description for TEST using the current description stack."
  (ert-bdd-build-description-stack test " "))

(defun ert-bdd-make-fn-name (test)
  "Build a name for TEST using the current description stack."
  (intern
   (replace-regexp-in-string
    (regexp-quote " ") "-"
    (ert-bdd-build-description-stack test "*"))))

(defmacro describe (description &rest body)
  (declare (indent 1)
           (debug (&define sexp def-body)))
  (setq ert-bdd-description-stack
        (cons description ert-bdd-description-stack))
  `(progn
     ,@body
     (setq ert-bdd-description-stack
           (cdr ert-bdd-description-stack))
     (ert-bdd-remove-from-setup-stack 'ert-bdd-before-stack ,description)
     (ert-bdd-remove-from-setup-stack 'ert-bdd-after-stack ,description)))

(defmacro it (description &rest body)
  (declare (indent 1)
           (debug (&define sexp def-body)))
  (let ((desc (ert-bdd-make-fn-desc description))
        (name (ert-bdd-make-fn-name description)))
    ;; `((,name ,desc ,@(ert-bdd-build-test-body body)))))
    `(ert-deftest ,name ()
       ,desc ,@(ert-bdd-build-test-body body))))

(defmacro before (&rest body)
  (declare (debug (&define def-body)))
  ;; TODO needs to build key from whole description stack, not just most recent
  (apply #'ert-bdd-add-to-setup-stack `(ert-bdd-before-stack ,@body)))

(defmacro after (&rest body)
  (declare (debug (&define def-body)))
  (apply #'ert-bdd-add-to-setup-stack `(ert-bdd-after-stack ,@body)))

(provide 'ert-bdd)
;;; ert-bdd.el ends here
