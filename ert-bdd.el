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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SHIMS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (functionp 'string-join)
    (defalias 'ert-bdd-string-join 'string-join)
  (defun ert-bdd-string-join (list sep)
    (mapconcat #'identity list sep)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;VARIABLES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst ert-bdd-description-separator "*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HELPERS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ert-bdd-describe-suite (suite)
  (message (pp suite)))

(defun ert-bdd-make-suite (description)
  (list (plist-put nil :description description)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DESCRIBE/IT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;DESCRIBE;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun describe-rebuild-body (body suite &optional result)
  (cond
   ((not body) result)
   ((-contains? '(describe it) (car body))
    ;; (it-nested
    (append (list (intern (concat (symbol-name (car body)) "-nested")))
            ;; "should whatever"
            (list (cadr body))
            ;; '((:describe "should whatever")
            ;;   (:describe "the thing" <before and after>))
            (list suite)
            ;; (expect t))
            (cddr body)))
   ((-contains? '(before after) (car body))
    (append (list (intern
                   (concat
                    (symbol-name (car body))
                    "-nested")))
            (list (cadr body))
            (list suite)
            (cddr body)))
   (t (describe-rebuild-body (car body) suite (append (cdr body) result)))))

(defmacro describe-nested (str suite &rest body)
  (let ((current-suite (append suite (ert-bdd-make-suite str)))
        ret)
    `((lambda (current-suite)
        ,@(dolist (item body ret)
            (setq ret (append
                       ret
                       (list (describe-rebuild-body item current-suite)))))) ',current-suite)))

(defmacro describe (str &rest body)
  (declare (indent 1))
  (let ((suite (ert-bdd-make-suite str))
        ret)
    `((lambda (current-suite)
        ,@(dolist (item body ret)
            (setq ret (append
                       ret
                       (list (describe-rebuild-body item suite)))))) ',suite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;IT;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro it-nested (str suite &rest body)
  (let* ((current-suite (append suite (ert-bdd-make-suite str)))

         (spec-desc (string-join (-map (lambda (pl) (plist-get pl :description))
                                       current-suite)
                                 ert-description-separator))
         (spec-name (intern (replace-regexp-in-string " " "-" spec-desc))))
    `((lambda (current-spec)
        (ert-deftest ,spec-name ()
          ,spec-desc
          ,@(append
             (thread-last current-suite
               (-map (lambda (pl) (plist-get pl :before-each)))
               (-filter #'identity)
               (apply #'append))
             body
             (thread-last current-suite
               (-map (lambda (pl) (plist-get pl :after-each)))
               (-filter #'identity)
               (apply #'append)
               reverse))))
      ',current-suite)))

(defmacro it (str &rest body)
  (declare (indent 1))
  (error "`it' must be nested in a `describe' form"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;BEFORE/AFTER;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;BEFORE;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro before-nested (key suite &rest body)
  (let ((new-key (intern (concat ":before-" (substring (symbol-name key) 1))))
        (all-but-last (-drop-last 1 suite))
        (last (car (-take-last 1 suite))))
    `(setq current-suite ',(append
                            all-but-last
                            (plist-put last new-key
                                       (append (plist-get last new-key) body))))))

(defmacro before (key &rest body)
  (declare (indent 1))
  (error "`before' must be nested in a `describe' form"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;AFTER;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro after-nested (key suite &rest body)
  (let ((new-key (intern (concat ":after-" (substring (symbol-name key) 1))))
        (all-but-last (-drop-last 1 suite))
        (last (car (-take-last 1 suite))))
    `(setq current-suite ',(append
                            all-but-last
                            (plist-put last new-key
                                       (append body (plist-get last new-key)))))))

(defmacro after (key &rest body)
  (declare (indent 1))
  (error "`after' must be nested in a `describe' form"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TEST CODE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;DESCRIBE/IT;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(describe "hey"
  (describe "ho"
    (describe "lets"
      (it "go"
        (ert-bdd-describe-suite current-suite))))

  (describe "you"
    (describe "get"
      (describe "offa"
        (describe "my"
          (it "cloud"
            (ert-bdd-describe-suite current-suite)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;BEFORE/AFTER;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(describe "whatevs"
  (before :each "before")
  (before :each "before2")
  (after :each "after")
  (after :each "after2")

  (it "things"
    (ert-bdd-describe-suite current-suite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EXPECT/MATCHERS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ert-bdd-matcher-alist nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;MATCHERS;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ert-bdd-lookup-matcher (matcher)
  (or (cadr (assoc matcher ert-bdd-matcher-alist))
      (error "Matcher %s not defined" (symbol-name matcher))))

(defun ert-bdd-expect (arg matcher args)
  (let ((func (ert-bdd-lookup-matcher matcher)))
    (apply func (cons arg args))))

(defun ert-bdd-have-same-items-p (a b)
  (let ((exclusive-a (-difference a b))
        (exclusive-b (-difference b a)))
    (not (or exclusive-a exclusive-b))))

(defun ert-bdd-are-close-p (a b tolerance)
  (< (abs (- a b)) tolerance))

(defmacro ert-bdd-add-matcher (keyword body)
  (declare (indent 1))
  `(setq
    ert-bdd-matcher-alist
    (cons
     (list ,keyword ,body)
     ert-bdd-matcher-alist)))

(defun ert-bdd-get-n-fn-matcher (n func arg-list &optional swap)
  (let* ((head `(lambda ,(append arg-list (list '&optional 'inverse))))
         (real-args
          (if (and (= 2 n) swap) (reverse arg-list) arg-list))
         (ret `((if inverse `(should-not (,',func ,@(list ,@real-args)))
                  `(should (,',func ,@(list ,@real-args)))))))
    (append head ret)))

(defmacro ert-bdd-add-n-fn-matcher (n keyword func &optional swap)
  (let ((arg-list (let (ret)
                    (dotimes (i n ret)
                      (let ((char-sym (list (intern (format "%c" (+ 97 i))))))
                        (setq ret (if ret (append ret char-sym) char-sym)))))))
    `(ert-bdd-add-matcher ,keyword
       ,(ert-bdd-get-n-fn-matcher n func arg-list swap))))

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
(ert-bdd-add-n-fn-matcher 3 :to-be-close-to        ert-bdd-are-close-p)

;; (ert-bdd-add-matcher :to-throw
;;   (lambda (expr &optional inverse)
;;     (if inverse (signal 'ert-bdd-not-implemented-error ":not :to-throw expr")
;;       (should-error expr))))

;; (when (not (get 'ert-bdd-error 'error-conditions))
;;   (define-error 'ert-bdd-error "An error has occurred executing a BDD spec")
;;   (define-error 'ert-bdd-not-implemented-error "Not yet implemented" 'ert-bdd-error))

(provide 'ert-bdd)
;;; ert-bdd.el ends here
