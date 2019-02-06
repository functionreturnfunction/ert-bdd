;;; ert-bdd.el --- BDD macros for `ert'. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jason Duncan, all rights reserved

;; Author:  Jason Duncan <jasond496@msn.com>
;; Version: 0.0 alpha2
;; URL: https://github.com/functionreturnfunction/ert-bdd
;; Package-Requires: ((emacs "24") (dash "2.15.0"))

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
(require 'dash)

(defconst ert-bdd-description-separator "*")

(defvar ert-bdd-description-stack nil)
(defvar ert-bdd-before-stack nil)
(defvar ert-bdd-after-stack nil)

(defvar ert-bdd-matcher-alist nil)

(progn
  (setq ert-bdd-description-stack nil)
  (setq ert-bdd-before-stack nil)
  (setq ert-bdd-after-stack nil)
  (setq ert-bdd-matcher-alist nil))

(if (functionp 'string-join)
    (defalias 'ert-bdd-string-join 'string-join)
  (defun ert-bdd-string-join (list sep)
    (mapconcat #'identity list sep)))

(defun ert-bdd-add-to-setup-stack (stack &rest body)
  "Add form BODY to setup STACK with a key generated by `ert-bdd-build-description-stack'."
  (set stack
       (append
        (symbol-value stack)
        `((,(ert-bdd-build-description-stack ert-bdd-description-separator) ,@body))))
  '(ignore))

(defun ert-bdd-remove-from-setup-stack (stack description)
  "Remove item from setup STACK with the key specified by DESCRIPTION."
  (set stack
       (--filter (string-equal description (car it)) (symbol-value stack))))

(defun ert-bdd-build-setup-stack (stack)
  "Return relrevant items from setup STACK given the current state of `ert-bdd-description-stack'."
  (let (part acc ret)
    (dolist (part (reverse ert-bdd-description-stack) ret)
      (setq acc (if acc (concat acc ert-bdd-description-separator part) part))
      (let ((cur (mapcar
                  #'cadr
                  (--filter
                   (string-equal (car it) acc) stack))))
        (when cur
          (setq ret (append ret cur)))))))

(defun ert-bdd-build-description-stack (sep)
  "Build `ert-bdd-description-stack' with SEP as a separator."
  (ert-bdd-string-join (reverse ert-bdd-description-stack) sep))

(defun ert-bdd-build-test-description (test sep)
  "Build a description for the current TEST using SEP to join the description stack."
  (ert-bdd-string-join `(,(ert-bdd-build-description-stack sep) ,test) sep))

(defun ert-bdd-build-test-body (test)
  "Build body of test by sandwiching TEST between relevant before and after lists."
  (append (ert-bdd-build-setup-stack ert-bdd-before-stack)
          test
          (reverse (ert-bdd-build-setup-stack ert-bdd-after-stack))))

(defun ert-bdd-make-fn-desc (test)
  "Build a description for TEST using the current description stack."
  (ert-bdd-build-test-description test " "))

(defun ert-bdd-make-fn-name (test)
  "Build a name for TEST using the current description stack."
  (intern
   (replace-regexp-in-string
    (regexp-quote " ") "-"
    (ert-bdd-build-test-description test ert-bdd-description-separator))))

(defmacro describe (description &rest body)
  "Describe a test suite.

DESCRIPTION is a string.  BODY is a sequence of instructions,
mainly calls to `describe', `it', `before', and `after'."
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
  "Define a spec.

DESCRIPTION is a string.  BODY is a sequence of instructions, most probably
including one or more calls to `should'."
  (declare (indent 1)
           (debug (&define sexp def-body)))
  (let ((desc (ert-bdd-make-fn-desc description))
        (name (ert-bdd-make-fn-name description)))
    `(ert-deftest ,name ()
       ,desc ,@(ert-bdd-build-test-body body))))

(defmacro before (&rest body)
  "Run BODY before each spec in the current suite."
  (declare (debug (&define def-body)))
  (apply #'ert-bdd-add-to-setup-stack `(ert-bdd-before-stack ,@body)))

(defmacro after (&rest body)
  "Run BODY after each spec in the current suite."
  (declare (debug (&define def-body)))
  (apply #'ert-bdd-add-to-setup-stack `(ert-bdd-after-stack ,@body)))

(defmacro expect (arg &optional matcher &rest args)
  (ert-bdd-expect arg (or matcher :to-be-truthy) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;MATCHERS;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ert-bdd-lookup-matcher (matcher)
  (or (cadr (assoc matcher ert-bdd-matcher-alist))
      (error "Matcher %s not defined" (symbol-name matcher))))

(defun ert-bdd-expect (arg matcher args)
  (let ((func (ert-bdd-lookup-matcher matcher)))
    `(,func ,@(cons arg args))))

(defun ert-bdd-have-same-items-p (a b)
  (let ((exclusive-a (-difference a b))
        (exclusive-b (-difference b a)))
    (not (or exclusive-a exclusive-b))))

(defmacro ert-bdd-add-matcher (keyword body)
  (declare (indent 1))
  `(setq
    ert-bdd-matcher-alist
    (cons
     (list ,keyword ,body)
     ert-bdd-matcher-alist)))

(defmacro ert-bdd-add-n-fn-matcher (n keyword func &optional swap)
  (let ((arg-list (let (ret)
                    (dotimes (i n ret)
                      (let ((char-sym (list (intern (format "%c" (+ 97 i))))))
                        (setq ret (if ret (append ret char-sym) char-sym)))))))
    `(ert-bdd-add-matcher ,keyword
       '(lambda ,(append arg-list
                         (list '&optional 'inverse))
          ,(when (and (= 2 n) swap)
             '(setq a (prog1 b (setq b a))))
          (if inverse (should-not (,func ,@arg-list))
            (should (,func ,@arg-list)))))))

(ert-bdd-add-matcher :not
  (lambda (obj matcher &rest args)
    (let ((func (ert-bdd-lookup-matcher matcher)))
      (apply func (append (list obj) args (list t))))))

(ert-bdd-add-n-fn-matcher 1 :to-be-truthy          identity)
(ert-bdd-add-n-fn-matcher 2 :to-be                 eq)
(ert-bdd-add-n-fn-matcher 2 :to-equal              equal)
(ert-bdd-add-n-fn-matcher 2 :to-be-less-than       <)
(ert-bdd-add-n-fn-matcher 2 :to-be-greater-than    >)
(ert-bdd-add-n-fn-matcher 2 :to-have-same-items-as ert-bdd-have-same-items-p)
(ert-bdd-add-n-fn-matcher 2 :to-match              string-match-p t)
(ert-bdd-add-n-fn-matcher 2 :to-contain            member t)

(ert-bdd-add-matcher :to-be-close-to
  (lambda (a b tolerance &optional inverse)
    (if inverse (should-not (< (abs (- a b)) tolerance))
      (should (< (abs (- a b)) tolerance)))))

(provide 'ert-bdd)
;;; ert-bdd.el ends here
