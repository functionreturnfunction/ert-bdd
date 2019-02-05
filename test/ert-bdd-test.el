;;; ert-bdd-test.el --- Tests for `ert-bdd'.         -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `ert-bdd'.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HELPER FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-testp (thing)
  (and (symbolp thing) (get thing 'ert--test)))

(defun get-ert-tests ()
  (seq-filter #'is-testp obarray))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UNIT TESTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(describe "ert-bdd-build-test-body"
  (it "builds the test with before and after statements in the proper order"
    (let* ((stack '(("foo" (foo))
                    ("foo*bar" (bar))
                    ("foo*bar*baz" (baz))))
           (ert-bdd-before-stack stack)
           (ert-bdd-after-stack stack)
           (ert-bdd-description-stack '("foo" "bar" "baz")))
      (should (equal '((foo) (bar) (baz) (test) (baz) (bar) (foo))
                     (ert-bdd-build-test-body '((test))))))))

(describe "ert-bdd-build-setup-stack"
  (it "builds the setup stack for relevant descriptions in the proper order"
    (let* ((stack '(("foo*bar" (bar))
                    ("foo" (foo))
                    ("foo*bar*baz" (baz))
                    ("not*releveant" (yuck))))
           (ert-bdd-description-stack '("foo" "bar" "baz"))
           (result (mapcar #'cdr (ert-bdd-build-setup-stack stack))))
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
