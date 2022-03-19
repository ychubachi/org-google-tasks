;;; -*- lexical-binding: t -*-

;; call

(require 'ht)
(require 'oauth2)

(defcustom my/gtasks-client-secret-json ""
  "JSON file location to client secret."
  :type '(string)
  :group 'my)

;;; GTasks API access utilities.
(defun my/token ()
  (let* ((secret (json-read-file my/gtasks-client-secret-json))
       (installed (cdr (assq 'installed secret)))
       (client-id (cdr (assq 'client_id installed)))
       (client-secret (cdr (assq 'client_secret installed)))
       (auth-url "https://accounts.google.com/o/oauth2/auth")
       (token-url "https://www.googleapis.com/oauth2/v3/token")
       (scope "https://www.googleapis.com/auth/tasks"))
    (oauth2-auth-and-store auth-url token-url scope client-id client-secret)))

(defun my/parse-http-response (buffer)
  "This function parses a buffer as an HTTP response. See RFC 2616.

This returns a plist of :status, :reason, :header and :body."
  (let (status reason message header body)
      ;; decode coding
      (with-current-buffer buffer
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

;;; Macro for Google Tasks API.
(defmacro my/api (url &optional request-method request-data)
  "This is a macro for REST API request.
REQUEST-DATA is any emacs lisp object that json-serialize understands."
  `(let* ((response-buffer (oauth2-url-retrieve-synchronously
			 (my/token)
			 ,url
			 ,request-method
			 (if ,request-data
			     (encode-coding-string (json-serialize ,request-data) 'utf-8))))
	  (response (my/parse-http-response response-buffer))
	  (body (decode-coding-string (plist-get response :body) 'utf-8)))
     (message body)
     (json-parse-string body)))

;;; Google Tasks APIs for tasklists.
(defun my/api-tasklists-list ()
  "Creates a new task list and adds it to the authenticated user's task lists.

See URL https://developers.google.com/tasks/reference/rest/v1/tasklists/list"
  (my/api "https://tasks.googleapis.com/tasks/v1/users/@me/lists"
	  "GET"))

(defun my/api-tasklists-insert (tasklist)
  "Creates a new task list and adds it to the authenticated user's task lists.

TASKLIST is a tasklist object. This returns response in JSON strings.
See URL https://developers.google.com/tasks/reference/rest/v1/tasklists/insert

Usage:
  (my/api-tasklists-insert '(:title \"My Tasklist\"))
  (my/api-tasklists-insert '((title . \"My Tasklist\")))
"
  (my/api "https://tasks.googleapis.com/tasks/v1/users/@me/lists"
	  "POST"
	  tasklist))

;; (my/api-tasklists-list)
;; (my/api-tasklists-insert '(:title "はひふへほ"))
;; (my/api-tasklists-insert '((title . "はひふ")))
;; (my/api "https://tasks.googleapis.com/tasks/v1/users/@me/lists" "POST" '(:title "さしすせそ"))
