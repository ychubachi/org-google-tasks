;;; test-helper.el --- Helpers for org-google-tasks-test.el -*- lexical-binding: t -*-
(message "Running tests on Emacs %s" emacs-version)

(require 'undercover)
(undercover "*.el"
            (:report-format 'lcov)
            (:send-report nil)
            (:exclude "org-sync-gtasks-api.el"))

(require 'org-sync-gtasks)
;;; test-helper.el ends here
