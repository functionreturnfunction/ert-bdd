;; -*- lexical-binding: t; -*-

(when (require 'undercover nil t)
  (undercover "*.el"
              (:exclude "*-tests.el")))

(require 'ert)
(require 'ert-bdd)
