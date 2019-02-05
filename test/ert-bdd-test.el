;;; ert-bdd-test.el --- Tests for `ert-bdd'.         -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `ert-bdd'.

;;; Code:

(ert-delete-all-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HELPER FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-testp (thing)
  (and (symbolp thing) (get thing 'ert--test)))

(defun get-ert-tests ()
  (-filter #'is-testp (mapcar #'identity obarray)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UNIT TESTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(describe "ert-bdd-add-to-setup-stack"
  (it "adds the body to the stack with the proper key"
    (let (ert-bdd-description-stack ert-bdd-before-stack)
      (setq ert-bdd-description-stack '("foo"))
      (ert-bdd-add-to-setup-stack 'ert-bdd-before-stack '(foo))

      (should (equal '(("foo" (foo))) ert-bdd-before-stack))

      (setq ert-bdd-description-stack '("bar" "foo"))
      (ert-bdd-add-to-setup-stack 'ert-bdd-before-stack '(bar))

      (should (equal '(("foo" (foo)) ("foo*bar" (bar))) ert-bdd-before-stack))

      (setq ert-bdd-description-stack '("baz" "bar" "foo"))
      (ert-bdd-add-to-setup-stack 'ert-bdd-before-stack '(baz))

      (should (equal '(("foo" (foo)) ("foo*bar" (bar)) ("foo*bar*baz" (baz)))
                     ert-bdd-before-stack)))))

(describe "ert-bdd-build-test-body"
  (it "builds the test with before and after statements in the proper order"
    (let* ((stack '(("foo" (foo))
                    ("foo*bar" (bar))
                    ("foo*bar*baz" (baz))))
           (ert-bdd-before-stack stack)
           (ert-bdd-after-stack stack)
           (ert-bdd-description-stack '("baz" "bar" "foo")))
      (should (equal '((foo) (bar) (baz) (test) (baz) (bar) (foo))
                     (ert-bdd-build-test-body '((test))))))))

(describe "ert-bdd-build-setup-stack"
  (it "builds the setup stack for relevant descriptions in the proper order"
    (let* ((stack '(("foo*bar" (bar))
                    ("foo" (foo))
                    ("foo*bar*baz" (baz))
                    ("not*releveant" (yuck))))
           (ert-bdd-description-stack '("baz" "bar" "foo"))
           (result (ert-bdd-build-setup-stack stack)))
      (should (equal '((foo) (bar) (baz)) result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;API TESTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(describe "describe"
  (it "should create tests with description as prefix and name as suffix"
    (should (seq-contains (get-ert-tests)
                          'describe*should-create-tests-with-description-as-prefix-and-name-as-suffix)))

  (describe "when nested"
    (it "should include nested descriptions"
      (should (seq-contains (get-ert-tests)
                            'describe*when-nested*should-include-nested-descriptions)))))

(let (foo)
  (describe "before"
    (before
     (setq foo "foo"))

    (after
     (setq foo nil))

    (it "runs before tests"
      (should (string-equal foo "foo")))

    (it "runs before all tests within the given scope"
      (should (string-equal foo "foo")))

    (describe "when nested"
      (before
       (setq foo "bar"))

      (it "applies in the correct order"
        (should (string-equal foo "bar"))))))

(describe "expect"
  (describe "in simplest form"
    (it "should check for non-nil"
      (expect t)))

  (describe "Included matchers:"
    (it "The :to-be matcher compares with `eq'"
      (let* ((a 12)
             (b a))
        (should (eq a b))
        ;; (expect a :not :to-be nil)))
        ))

    (describe "The :to-equal matcher"
      (it "works for simple literals and variables"
        (let ((a 12))
          (expect a :to-equal 12)))

      (it "should work for compound objects"
        (let ((foo '((a . 12) (b . 34)))
              (bar '((a . 12) (b . 34))))
          (expect foo :to-equal bar))))
    ))

;;   (it "The :to-have-same-items-as matcher compares two lists as sets"
;;     (let ((first (list "a" "b" "c"))
;;           (second (list "c" "a" "b"))
;;           (third (list "a" "c" "d"))
;;           (fourth (list "a" "b")))
;;       (expect first :to-have-same-items-as second)
;;       (expect second :to-have-same-items-as first)
;;       (expect first :not :to-have-same-items-as third)
;;       (expect third :not :to-have-same-items-as second)
;;       (expect first :not :to-have-same-items-as fourth)
;;       (expect fourth :not :to-have-same-items-as first)))

;;   (it "The :to-match matcher is for regular expressions"
;;     (let ((message "foo bar baz"))
;;       (expect message :to-match "bar")
;;       (expect message :to-match (rx "bar"))
;;       (expect message :not :to-match "quux")))

;;   (it "The :to-be-truthy matcher is for boolean casting testing"
;;     (let (a
;;           (foo "foo"))
;;       (expect foo :to-be-truthy)
;;       (expect a :not :to-be-truthy)))

;;   (it "The :to-contain matcher is for finding an item in a list"
;;     (let ((a '("foo" "bar" "baz")))
;;       (expect a :to-contain "bar")
;;       (expect a :not :to-contain "quux")))

;;   (it "The :to-be-less-than matcher is for mathematical comparisons"
;;     (let ((pi 3.1415926)
;;           (e 2.78))
;;       (expect e :to-be-less-than pi)
;;       (expect pi :not :to-be-less-than e)))

;;   (it "The :to-be-greater-than matcher is for mathematical comparisons"
;;     (let ((pi 3.1415926)
;;           (e 2.78))
;;       (expect pi :to-be-greater-than e)
;;       (expect e :not :to-be-greater-than pi)))

;;   (it "The :to-be-close-to matcher is for precision math comparison"
;;     (let ((pi 3.1415926)
;;           (e 2.78))
;;       (expect pi :not :to-be-close-to e 2)
;;       (expect pi :to-be-close-to e 0)))

;;   (describe "The :to-throw matcher"
;;     (it "is for testing if an expression throws an exception"
;;       (expect (+ 1 2) :not :to-throw)
;;       (expect (+ a 1) :to-throw))
;;     (it "accepts a symbol to check for the signal thrown"
;;       (expect (/ 1 0) :not :to-throw 'void-variable)
;;       (expect (+ a 1) :to-throw 'void-variable))
;;     (it "optionally matches arguments to signals"
;;       (expect (+ a 1) :not :to-throw 'void-variable '(b))
;;       (expect (+ a 1) :to-throw 'void-variable '(a))))))
