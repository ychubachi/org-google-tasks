;;; -*- lexical-binding: t -*-

;; call

(straight-use-package 'oauth2)
(require 'oauth2)

(defcustom my/gtasks-client-secret-json ""
  "JSON file location to client secret."
  :type '(string)
  :group 'my)

(defun my/token ()
  (let* ((secret (json-read-file my/gtasks-client-secret-json))
       (installed (cdr (assq 'installed secret)))
       (client-id (cdr (assq 'client_id installed)))
       (client-secret (cdr (assq 'client_secret installed)))
       (auth-url "https://accounts.google.com/o/oauth2/auth")
       (token-url "https://www.googleapis.com/oauth2/v3/token")
       (scope "https://www.googleapis.com/auth/tasks"))
    (oauth2-auth-and-store auth-url token-url scope client-id client-secret)))

;; parse RFC2616
(defun my/parse-http-response (buffer)
  (let (status reason message header body)
      ;; decode coding
      (with-current-buffer buffer
        ;; Debug
        ;; (with-output-to-temp-buffer "*yc temp out*"
        ;;   (princ (buffer-string)))
        ;; parse RFC2616
        (goto-char (point-min))
        ;; status-line
        (looking-at "^HTTP/[^ ]+ \\([0-9]+\\) ?\\(.*\\)$")
        (setq status (string-to-number (match-string 1)))
        (setq reason (match-string 2))
        (forward-line)
        ;; headers
        (while (not (looking-at "^$"))
          (looking-at "^\\([^:]+\\): \\(.*\\)$")
          (push (cons (match-string 1) (match-string 2)) header)
          (forward-line))
        ;; CRLF
        (forward-line)
        ;; message-body
        (setq body (buffer-substring (point) (point-max)))
        ;; return results
        (list :status status :reason reason :header header :body body))))

(defun my/tasklists-insert (title)
  "Creates a new task list and adds it to the authenticated user's task lists."
  (let* ((response-buffer (oauth2-url-retrieve-synchronously
			 (my/token)
			 "https://tasks.googleapis.com/tasks/v1/users/@me/lists"
			 "POST"
			 (format "{\"title\": \"%s\"}" (encode-coding-string title 'utf-8))))
       (response (my/parse-http-response response-buffer)))
    (decode-coding-string (plist-get response :body) 'utf-8)))

;; (my/tasklists-insert "My New Tasklist")
