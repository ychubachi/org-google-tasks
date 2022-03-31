;;; usdercover-lcov.el --- Helpers for org-google-tasks-test.el -*- lexical-binding: t -*-
(message "Use lcov for coverage")

(require 'undercover)
(undercover "org-sync-gtasks.el"
            (:report-format 'lcov)
            (:send-report nil)
            (:exclude "org-sync-gtasks-api.el"))
;;; usdercover-lcov.el ends here
