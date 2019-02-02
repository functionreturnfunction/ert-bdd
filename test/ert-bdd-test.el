;;; ert-bdd-test.el --- Tests for `ert-bdd'.         -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `ert-bdd'.

;;; Code:

(require 'ert)
(require 'ert-bdd)

(describe "describe"
  (it "should create tests with the description as prefix and name as suffix"
    (should
     (seq-contains (seq-filter
                    (lambda (elt) (and (symbolp elt) (ert-test-boundp elt))) obarray)
                   'describe-should-create-tests-with-the-description-as-prefix-and-name-as-suffix
                   )))

  (describe "when nested"
    (it "should include nested descriptions"
      (seq-contains (seq-filter
                     (lambda (elt) (and (symbolp elt) (ert-test-boundp elt))) obarray)
                    'describe-when-nested-should-include-nested-descriptions
                    ))))

(provide 'ert-bdd-test)
;;; ert-bdd-test.el ends here
