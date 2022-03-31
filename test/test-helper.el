;;; test-helper.el --- Helpers for org-google-tasks-test.el -*- lexical-binding: t -*-
(message "Running tests on Emacs %s" emacs-version)

;; (when (require 'undercover nil t)
;;   )

(require 'undercover)
(undercover "org-sync-gtasks.el"
            (:report-format 'lcov)
            (:send-report nil)
            (:exclude "org-sync-gtasks-api.el"))

;; (setq undercover-force-coverage t)
;; (undercover "org-sync-gtasks.el" (:report-format 'simplecov)
;;             (:send-report nil))

(require 'org-sync-gtasks)
;;; test-helper.el ends here
