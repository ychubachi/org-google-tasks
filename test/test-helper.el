;;; test-helper.el --- Helpers for org-google-tasks-test.el
(message "Running tests on Emacs %s" emacs-version)

(when  (require 'undercover)
  (undercover "*.el"
              (:report-format 'lcov)
              (:send-report nil)))

(require 'org-sync-gtasks)
;;; test-helper.el ends here
