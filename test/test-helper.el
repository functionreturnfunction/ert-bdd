;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name ".cask/$(emacs-version)/elpa"))

(when (require 'undercover nil t)
  (undercover "*.el"
              (:exclude "*-tests.el")))

(require 'ert)
(require 'ert-bdd)
