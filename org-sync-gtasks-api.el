;;; org-sync-gtasks-api.el --- Google Tasks API fuctions -*- lexical-binding: t -*-

(require 'oauth2)
(require 'ht)

;;; Google Tasks API related functions.
(defcustom org-sync-gtasks-client-secret-json "~/.client_secret.json" ; TODO: --
  "The location to your client secret JSON file for GTasks."
  :type '(string)
  :group 'my)

(defun org-sync-gtasks--token ()
  (let* ((secret (json-read-file org-sync-gtasks-client-secret-json))
         (installed (cdr (assq 'installed secret)))
         (client-id (cdr (assq 'client_id installed)))
         (client-secret (cdr (assq 'client_secret installed)))
         (auth-url "https://accounts.google.com/o/oauth2/auth")
         (token-url "https://www.googleapis.com/oauth2/v3/token")
         (scope "https://www.googleapis.com/auth/tasks"))
    (oauth2-auth-and-store auth-url token-url scope client-id client-secret)))

(defun org-sync-gtasks--parse-http-response (buffer)
  "This function parses a buffer as an HTTP response. See RFC 2616.

This returns a plist of :status, :reason, :header and :body."
  (let (status reason header body)
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

(defmacro org-sync-gtasks--api (url &optional request-method request-data)
  "This is a macro for Google Tasks API request.
REQUEST-DATA is any emacs lisp object that json-serialize understands.

Usage: TODO
"
  `(let* ((result)
          (response-buffer
           (oauth2-url-retrieve-synchronously
	    (org-sync-gtasks--token)
	    ,url
	    ,request-method
	    (if ,request-data
		(encode-coding-string (json-serialize ,request-data) 'utf-8))))
	  (response (org-sync-gtasks--parse-http-response response-buffer))
          (status (plist-get response :status))
          (reason (plist-get response :reason))
	  (body (decode-coding-string (plist-get response :body) 'utf-8)))
     ;; (message "org-sync-gtasks--api %s %s" ,request-method ,url)
     ;; Error in HTTP response
     (if (not (eq status 200))
         (progn
           (error
            "GTask HTTP error: %s\nurl=%s,\nrequest method=%s\nrequest data=%s"
            reason ,url ,request-method ,request-data)))
     :status status :reason reason
     ;; Error in HTTP response body
     (setq result (json-parse-string body))
     ;; Errors in JSON response
     (let ((error-data (ht-get result "error")))
       (if error-data
           (error "GTask API error: %s (%s)"
                  (ht-get error-data "message")
                  error-data))) ;; TODO: more Echo datail?
     result))

(defun org-sync-gtasks--api-tasklists-list ()
  "Creates a new task list and adds it to the authenticated user's task lists.

See URL https://developers.google.com/tasks/reference/rest/v1/tasklists/list"
  (org-sync-gtasks--api "https://tasks.googleapis.com/tasks/v1/users/@me/lists"
	                "GET"))

;; Test case:
;; (org-sync-gtasks--api-tasklists-list)
;; => #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#taskLists" "etag" "\"LTczMzUyMzE3Nw\"" "items" [#s(hash-table size 6 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#taskList" "id" "MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow" "etag" "\"LTE0MDA4ODA3NTY\"" "title" "Tasklist" "updated" "2022-03-21T12:42:40.530Z" "selfLink" "https://www.googleapis.com/tasks/v1/users/@me/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow"))]))

(defun org-sync-gtasks--api-tasklists-insert (tasklist-id)
  "Creates a new task list and adds it to the authenticated user's task lists.

TASKLIST is a tasklist object. This returns response in JSON strings.
See URL https://developers.google.com/tasks/reference/rest/v1/tasklists/insert

Usage:
  (org-sync-gtasks--api-tasklists-insert '(:title \"My Tasklist\"))
  (org-sync-gtasks--api-tasklists-insert '((title . \"My Tasklist\")))
"
  (org-sync-gtasks--api "https://tasks.googleapis.com/tasks/v1/users/@me/lists"
	                "POST"
	                tasklist-id))

;; Test Case:
;; (org-sync-gtasks--api-tasklists-list)
;; => #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#taskLists" "etag" "\"MjMzMzgxMDU4\"" "items" [#s(hash-table size 6 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#taskList" "id" "MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow" "etag" "\"LTEzOTEwMzYxODY\"" "title" "Tasklist" "updated" "2022-03-21T15:26:45.688Z" "selfLink" "https://www.googleapis.com/tasks/v1/users/@me/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow"))]))

;; (org-sync-gtasks--api-tasklists-insert '(:title "My Tasklist"))
;; => #s(hash-table size 6 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#taskList" "id" "SjVxWVYyS0V6VS1qbHZvQQ" "etag" "\"LTEzNDU3MDgwNzk\"" "title" "My Tasklist" "updated" "2022-03-22T04:02:13.325Z" "selfLink" "https://www.googleapis.com/tasks/v1/users/@me/lists/SjVxWVYyS0V6VS1qbHZvQQ"))

;; (org-sync-gtasks--api-tasklists-insert '(:title "楽しいお仕事一覧"))
;; => #s(hash-table size 6 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#taskList" "id" "dUxIS2F5ckxWTnlsa3NvRQ" "etag" "\"LTEzNDU2Nzc3OTY\"" "title" "楽しいお仕事一覧" "updated" "2022-03-22T04:02:43.458Z" "selfLink" "https://www.googleapis.com/tasks/v1/users/@me/lists/dUxIS2F5ckxWTnlsa3NvRQ"))

(defun org-sync-gtasks--api-tasks-list (tasklist)
  "Returns all tasks in the specified task list.

See URL https://developers.google.com/tasks/reference/rest/v1/tasks/list"
  (org-sync-gtasks--api
   (format "https://tasks.googleapis.com/tasks/v1/lists/%s/tasks" tasklist)
   "GET"))

;; Test case:
;; (org-sync-gtasks--api-tasks-list "MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow") =>
;; #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#tasks" "etag" "\"LTE0MDA4ODA3NTY\"" "items" [#s(hash-table size 8 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#task" "id" "bWRqWnRLVy1fU1JyU0lVSA" "etag" "\"LTE0MDMxNjI0NjY\"" "title" "headline" "updated" "2022-03-21T12:04:39.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/bWRqWnRLVy1fU1JyU0lVSA" "position" "00000000000000000000" "status" "needsAction")) #s(hash-table size 9 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#task" "id" "YWlqV0hsRV9lYVlQdkx5MQ" "etag" "\"LTIxMDQ5NTgyODI\"" "title" "Task2 with note" "updated" "2022-03-13T09:08:03.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/YWlqV0hsRV9lYVlQdkx5MQ" "position" "00000000000000000002" "notes" "This is note string.
;; Hello World!" "status" "needsAction")) #s(hash-table size 9 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#task" "id" "dlAzdXRlWDh2Z0dsck4xcQ" "etag" "\"MjA3MjIxMDY4Nw\"" "title" "Task 7 as sub-task" "updated" "2022-03-12T00:24:45.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/dlAzdXRlWDh2Z0dsck4xcQ" "parent" "aEQ1TjhNOGRiS3p6VmR4dw" "position" "00000000000000000000" "status" "needsAction")) #s(hash-table size 8 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#task" "id" "aEQ1TjhNOGRiS3p6VmR4dw" "etag" "\"MjA3MjE3NDc0OA\"" "title" "Task6 as parent" "updated" "2022-03-12T00:24:09.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/aEQ1TjhNOGRiS3p6VmR4dw" "position" "00000000000000000006" "status" "needsAction")) #s(hash-table size 9 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#task" "id" "Y1hxLXB0ZHJZb0x0Z3I0Mw" "etag" "\"MjA3MjE1ODc0MQ\"" "title" "Task5 with date time" "updated" "2022-03-12T00:23:53.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/Y1hxLXB0ZHJZb0x0Z3I0Mw" "position" "00000000000000000005" "status" "needsAction" "due" "2022-03-15T00:00:00.000Z")) #s(hash-table size 9 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#task" "id" "aUhGRlpRMXhqbmZrN1JsQQ" "etag" "\"MjA3MjE0NDc4NQ\"" "title" "Task4 with date (repeat)" "updated" "2022-03-12T00:23:38.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/aUhGRlpRMXhqbmZrN1JsQQ" "position" "00000000000000000004" "status" "needsAction" "due" "2022-03-15T00:00:00.000Z")) #s(hash-table size 9 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#task" "id" "TDlONFk0TThJT1VGb0h6RQ" "etag" "\"MjA3MjA4NTAzMA\"" "title" "Tasks3 with date" "updated" "2022-03-12T00:22:39.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/TDlONFk0TThJT1VGb0h6RQ" "position" "00000000000000000003" "status" "needsAction" "due" "2022-03-31T00:00:00.000Z")) #s(hash-table size 8 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#task" "id" "cF9ORW8yYWgyNTVES1dIbg" "etag" "\"MjA3MjA0NDc2MQ\"" "title" "Task1" "updated" "2022-03-12T00:21:59.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/cF9ORW8yYWgyNTVES1dIbg" "position" "00000000000000000001" "status" "needsAction"))]))

(defun org-sync-gtasks--api-tasks-insert (tasklist-id task)
  "TODO: Creates a new task list and adds it to the authenticated user's task lists.

TASKLIST is a tasklist object. This returns response in JSON strings.
See URL https://developers.google.com/tasks/reference/rest/v1/tasks/insert

Usage:
  (org-sync-gtasks--api-tasks-insert '(:title \"My Task\"))
  (org-sync-gtasks--api-tasks-insert '((title . \"My Task\")))
"
  (org-sync-gtasks--api
   (format "https://tasks.googleapis.com/tasks/v1/lists/%s/tasks" tasklist-id)
   "POST"
   task))
;; Test case:

;; (org-sync-gtasks--api-tasks-insert "MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow" '(:title "My Task"))
;; => #s(hash-table size 8 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#task" "id" "dW1RS0Q5ZENSd2ZnbUQ1Uw" "etag" "\"LTE0MDQ4NjU5MDg\"" "title" "My Task" "updated" "2022-03-21T11:36:16.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/dW1RS0Q5ZENSd2ZnbUQ1Uw" "position" "00000000000000000000" "status" "needsAction"))

;; (org-sync-gtasks--api-tasks-insert "HOGE" '(:title "My Task"))
;; => error

(defun org-sync-gtasks--api-tasks-get (tasklist-id task-id)
  "TODO: Creates a new task list and adds it to the authenticated user's task lists.

TASKLIST is a tasklist object. This returns response in JSON strings.
See URL https://developers.google.com/tasks/reference/rest/v1/tasks/insert

Usage:
  (org-sync-gtasks--api-tasks-get tasklist-id task-id)
"
  (org-sync-gtasks--api
   (format "https://tasks.googleapis.com/tasks/v1/lists/%s/tasks/%s" tasklist-id task-id)
   "GET"))

;; Test Case:
;; (org-sync-gtasks--api-tasks-get "MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow" "dW1RS0Q5ZENSd2ZnbUQ1Uw")
;; => #s(hash-table size 9 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#task" "id" "dW1RS0Q5ZENSd2ZnbUQ1Uw" "etag" "\"LTE0MDM0NzQyNjY\"" "title" "My Task" "updated" "2022-03-21T11:59:27.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/dW1RS0Q5ZENSd2ZnbUQ1Uw" "position" "00000000000000000000" "status" "needsAction" "deleted" t))

(defun org-sync-gtasks--api-tasks-patch (tasklist-id task-id task)
  "TODO: Creates a new task list and adds it to the authenticated user's task lists.

TASKLIST is a tasklist object. This returns response in JSON strings.
See URL https://developers.google.com/tasks/reference/rest/v1/tasks/insert

Usage:
  (org-sync-gtasks--api-tasks-insert '(:title \"My Task\"))
  (org-sync-gtasks--api-tasks-insert '((title . \"My Task\")))
"
  (org-sync-gtasks--api
   (format "https://tasks.googleapis.com/tasks/v1/lists/%s/tasks/%s" tasklist-id task-id)
   "PATCH"
   task))
;; Test case:

;; (org-sync-gtasks--api-tasks-patch "MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow"
;;                                   "bmFJZ2pFbGhQUXU0OU1xYQ"
;;                                   '(:title "Important Task"))

;; => #s(hash-table size 8 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#task" "id" "bmFJZ2pFbGhQUXU0OU1xYQ" "etag" "\"LTEzOTM3MjY2NzA\"" "title" "My Task あいうお" "updated" "2022-03-21T14:41:54.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/bmFJZ2pFbGhQUXU0OU1xYQ" "position" "00000000000000000000" "status" "needsAction"))

;;
;; => #s(hash-table size 8 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#task" "id" "dW1RS0Q5ZENSd2ZnbUQ1Uw" "etag" "\"LTE0MDQ4NjU5MDg\"" "title" "My Task" "updated" "2022-03-21T11:36:16.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/dW1RS0Q5ZENSd2ZnbUQ1Uw" "position" "00000000000000000000" "status" "needsAction"))

;; (org-sync-gtasks--api-tasks-insert "HOGE" '(:title "My Task"))
;; => error

(provide 'org-sync-gtasks-api)
;;; org-sync-gtasks-api.el ends here
