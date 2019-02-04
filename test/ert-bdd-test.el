;;; ert-bdd-test.el --- Tests for `ert-bdd'.         -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `ert-bdd'.

;;; Code:

(defun is-testp (thing)
  (and (symbolp thing) (get thing 'ert--test)))

(defun get-ert-tests ()
  (seq-filter #'is-testp obarray))

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
