;;; ert-bdd-test.el --- Tests for `ert-bdd'.         -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `ert-bdd'.

;;; Code:

(defun is-testp (thing)
  (and (symbolp thing) (ert-test-boundp thing)))

(defun ert-testp (symbol)
  (seq-contains (seq-filter #'is-testp obarray) symbol))

(describe "describe"
  (it "should create tests with description as prefix and name as suffix"
    (should (ert-testp 'describe*should-create-tests-with-description-as-prefix-and-name-as-suffix)))

  (describe "when nested"
    (it "should include nested descriptions"
      (should (ert-testp 'describe*when-nested*should-include-nested-descriptions)))))
