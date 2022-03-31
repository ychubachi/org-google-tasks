;;; usdercover-simplecov.el --- Helpers for org-google-tasks-test.el -*- lexical-binding: t -*-
(message "Use simplecov for coverage")

(require 'undercover)
(setq undercover-force-coverage t)
(undercover "org-sync-gtasks.el" (:report-format 'simplecov)
            (:send-report nil))
;;; usdercover-simplecov.el ends here
