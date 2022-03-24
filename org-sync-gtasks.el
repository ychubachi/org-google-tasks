;;; -*- lexical-binding: t -*-

;; call
(require 'ht)
(require 'oauth2)

;;; For tests.
(require 'el-mock)

(defcustom org-sync-gtasks--gtasks-client-secret-json "~/.client_secret.json"
  "The location to your client secret JSON file for GTasks."
  :type '(string)
  :group 'my)

;;; GTasks API access utilities.
(defun org-sync-gtasks--token ()
  (let* ((secret (json-read-file org-sync-gtasks--gtasks-client-secret-json))
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

;;; Google Tasks APIs for tasklists/tasks.
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
                  error-data)))
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
   (format
    "https://tasks.googleapis.com/tasks/v1/lists/%s/tasks" tasklist)
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

;;; Defult tasklist.
(defun org-sync-gtasks--default-tasklist-id ()
  "This gets the defult tasklist ID."
  (ht-get
   (aref (ht-get (org-sync-gtasks--api-tasklists-list) "items") 0)
   "id"))

(ert-deftest org-sync-gtasks--default-tasklist-id-test ()
  (with-mock
    (stub org-sync-gtasks--api-tasklists-list =>
          (ht ("items" `[,(ht ("id" "SOME-GTASKLIST-ID"))])))
    (let ((result (org-sync-gtasks--default-tasklist-id)))
      (should (equal result "SOME-GTASKLIST-ID")))))

;;; Task API related functions TODO: Unnecessary? to make stub?
(defun org-sync-gtasks--insert-task (tasklist-id task) ;; TODO: Unnecessary?
  "returns new task id."
  (ht-get (org-sync-gtasks--api-tasks-insert tasklist-id task) "id"))

(defun org-sync-gtasks--patch-task (tasklist-id task-id task)
  "returns task id"
  (ht-get (org-sync-gtasks--api-tasks-patch tasklist-id task-id task) "id"))

(defun org-sync-gtasks--get-task-etag (tasklist-id task-id)
  "returns task ETAG"
  (ht-get (org-sync-gtasks--api-tasks-get tasklist-id task-id) "etag"))

;;; Macro to test for changing org file.
(defmacro org-sync-gtasks--test-with-org-buffer (&rest rest)
  "A macro to write test functions which changes org texts.

REST is a plist.
:input input org text
:output output org text
 :target target body to test."
  (let ((target (plist-get rest :target))
        (list (plist-get rest :list)))
    ;; If only one example pair, make them to list.
    (if (eq list nil)
        (progn
          (let ((input (plist-get rest :input))
                (output (plist-get rest :output)))
            (setq list (list (list :input input :output output))))))
    ;; Iterate each example pair list.
    (setq r (mapcar (lambda (x)
                      (let ((input (plist-get x :input))
                            (output (plist-get x :output)))
                        `(let* ((org ,input)
	                        (result))
                           (with-current-buffer
                               (find-file-noselect (make-temp-file "org"))
	                     (org-mode)
	                     (insert org)
	                     ;; target
                             ,target
	                     ;; test
	                     (setq result (substring-no-properties (buffer-string)))
	                     (set-buffer-modified-p nil)
	                     (kill-buffer (current-buffer)))
                           (should (string-match ,output result))))
                      )
                    list))
    (push 'progn r)))

(ert-deftest org-sync-gtasks--test-with-org-buffer-test()
  "Test org-sync-gtasks--test-with-org-buffer macro for development purpose."
  (with-mock
    (stub org-id-new => "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
    (org-sync-gtasks--test-with-org-buffer
     :input
     "* headline
"
     :output
     "* headline
:PROPERTIES:
:ID:       xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
:END:
"
     :target
     (org-id-get-create))))

;;;

;;; get-tasklist-id
(defun org-sync-gtasks--get-or-put-tasklist-id () ; TODO: --headlie-get-or-put-tasklist-id
  "Get tasklist id. If it doesn't have GTASKLIST-ID, get it from GTasks."
  (let ((gtasklist-id (org-entry-get nil "GTASKS-TASKLIST-ID"))) ; GTASKS-TASKLIST-ID
    (if (eq gtasklist-id nil)
        (progn
          (setq gtasklist-id (org-sync-gtasks--default-tasklist-id) )
          (org-entry-put nil "GTASKS-TASKLIST-ID" gtasklist-id)))
    gtasklist-id))

(ert-deftest org-sync-gtasks--get-or-create-tasklist-id-test ()
  (with-mock
    (stub org-sync-gtasks--default-tasklist-id => "NEW-GTASKS-TASKLIST-ID")
    (org-sync-gtasks--test-with-org-buffer
     :target
     (org-sync-gtasks--get-or-put-tasklist-id)
     :list
     ((:input
       "* headline
"
       :output
       "* headline
:PROPERTIES:
:GTASKS-TASKLIST-ID: NEW-GTASKS-TASKLIST-ID
:END:
")
      (:input
       "* headline
:PROPERTIES:
:GTASKS-TASKLIST-ID: SOME-GTASKS-TASKLIST-ID
:END:
"
       :output
       "* headline
:PROPERTIES:
:GTASKS-TASKLIST-ID: SOME-GTASKS-TASKLIST-ID
:END:
")))))

;; TODO:
;;; Pull Gtasks task to org file.
(defun org-sync-gtasks--make-id-to-gtask-table ()
  "Create a hash table for looking up tasklist or task items by the id."
  (let* ((table (ht-create))
	 (tasklists (org-sync-gtasks--api-tasklists-list))
	 (tasklists-items (ht-get tasklists "items")))
    (dolist (tasklist (cl-coerce tasklists-items 'list))
      (let* ((tasklist-id (ht-get tasklist "id")))
		;; Set the tasklist to the table.
		;; (ht-set! table tasklist-id tasklist)
		;; Get the tasks under the tasklist.
		(let* ((tasks (org-sync-gtasks--api-tasks-list tasklist-id))
		       (tasks-items (ht-get tasks "items")))
		  (dolist (task (cl-coerce tasks-items 'list))
		    (let ((task-id (ht-get task "id")))
		       ;; Set the task to the table.
		       (ht-set! table task-id task))))))
    table))

(ert-deftest org-sync-gtasks--make-id-to-gtask-table-test ()
  (with-mock
    (stub org-sync-gtasks--api-tasklists-list =>
	  #s(hash-table test equal data
                        ("kind" "tasks#taskLists" "etag" "\"MjA0MzIwOTcyNw\"" "items" [#s(hash-table test equal data ("kind" "tasks#taskList" "id" "MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow" "etag" "\"LTE1Nzc5OTMyNDA\"" "title" "Tasklist" "updated" "2022-03-19T11:30:48.118Z" "selfLink" "https://www.googleapis.com/tasks/v1/users/@me/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow")) #s(hash-table test equal data ("kind" "tasks#taskList" "id" "MkhZNnRBVGE4eFk2UW1sdw" "etag" "\"LTE4NTg3MTY1ODQ\"" "title" "タスク" "updated" "2022-03-16T05:32:05.318Z" "selfLink" "https://www.googleapis.com/tasks/v1/users/@me/lists/MkhZNnRBVGE4eFk2UW1sdw")) #s(hash-table test equal data ("kind" "tasks#taskList" "id" "M21GQnJNQm84YXdCWVZRcw" "etag" "\"LTE1NzA3ODUwNzE\"" "title" "はひふへほ" "updated" "2022-03-19T13:30:56.845Z" "selfLink" "https://www.googleapis.com/tasks/v1/users/@me/lists/M21GQnJNQm84YXdCWVZRcw"))])))
    (stub org-sync-gtasks--api-tasks-list =>
	  #s(hash-table test equal data
			("kind" "tasks#tasks" "etag" "\"LTE1Nzc5OTMyNDA\"" "items" [#s(hash-table size 9 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#task" "id" "YWlqV0hsRV9lYVlQdkx5MQ" "etag" "\"LTIxMDQ5NTgyODI\"" "title" "Task2 with note" "updated" "2022-03-13T09:08:03.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/YWlqV0hsRV9lYVlQdkx5MQ" "position" "00000000000000000001" "notes" "This is note string.
Hello World!" "status" "needsAction")) #s(hash-table test equal data ("kind" "tasks#task" "id" "dlAzdXRlWDh2Z0dsck4xcQ" "etag" "\"MjA3MjIxMDY4Nw\"" "title" "Task 7 as sub-task" "updated" "2022-03-12T00:24:45.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/dlAzdXRlWDh2Z0dsck4xcQ" "parent" "aEQ1TjhNOGRiS3p6VmR4dw" "position" "00000000000000000000" "status" "needsAction")) #s(hash-table test equal data ("kind" "tasks#task" "id" "aEQ1TjhNOGRiS3p6VmR4dw" "etag" "\"MjA3MjE3NDc0OA\"" "title" "Task6 as parent" "updated" "2022-03-12T00:24:09.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/aEQ1TjhNOGRiS3p6VmR4dw" "position" "00000000000000000005" "status" "needsAction")) #s(hash-table test equal data ("kind" "tasks#task" "id" "Y1hxLXB0ZHJZb0x0Z3I0Mw" "etag" "\"MjA3MjE1ODc0MQ\"" "title" "Task5 with date time" "updated" "2022-03-12T00:23:53.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/Y1hxLXB0ZHJZb0x0Z3I0Mw" "position" "00000000000000000004" "status" "needsAction" "due" "2022-03-15T00:00:00.000Z")) #s(hash-table test equal data ("kind" "tasks#task" "id" "aUhGRlpRMXhqbmZrN1JsQQ" "etag" "\"MjA3MjE0NDc4NQ\"" "title" "Task4 with date (repeat)" "updated" "2022-03-12T00:23:38.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/aUhGRlpRMXhqbmZrN1JsQQ" "position" "00000000000000000003" "status" "needsAction" "due" "2022-03-15T00:00:00.000Z"))])))
    (let* ((table (org-sync-gtasks--make-id-to-gtask-table))
          (result (ht-keys table)))
      (should (equal
	       (format "%s" result)
               "(aUhGRlpRMXhqbmZrN1JsQQ Y1hxLXB0ZHJZb0x0Z3I0Mw aEQ1TjhNOGRiS3p6VmR4dw dlAzdXRlWDh2Z0dsck4xcQ YWlqV0hsRV9lYVlQdkx5MQ)"
	       ;; "(M21GQnJNQm84YXdCWVZRcw MkhZNnRBVGE4eFk2UW1sdw aUhGRlpRMXhqbmZrN1JsQQ Y1hxLXB0ZHJZb0x0Z3I0Mw aEQ1TjhNOGRiS3p6VmR4dw dlAzdXRlWDh2Z0dsck4xcQ YWlqV0hsRV9lYVlQdkx5MQ MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow)"
               )))))

;;; Make hedline
(defun org-sync-gtasks--insert-todo-headline (tasklist-id gtask) ; TODO: --headline-insert..
  "Make a new todo headline from GTasks' task."
  (org-insert-todo-heading-respect-content)
  (org-edit-headline (ht-get gtask "title"))
  (org-sync-gtasks--update-todo-headline tasklist-id gtask))

(defun org-sync-gtasks--update-todo-headline (tasklist-id gtask) ; TODO: --headline-update..
  "Update headline and its properties."
  (org-edit-headline (ht-get gtask "title"))
  (org-entry-put nil "GTASKS-TASKLIST-ID" tasklist-id)
  (org-entry-put nil "GTASKS-ID" (ht-get gtask "id"))
  (org-entry-put nil "GTASKS-ETAG" (ht-get gtask "etag"))
  (if (ht-get gtask "parent")
      (org-entry-put nil "GTASKS-PARENT" (ht-get gtask "parent")))
  (if (ht-get gtask "notes")
      (org-entry-put nil "GTASKS-NOTES" (ht-get gtask "notes")))
  (if (ht-get gtask "due")
      (org-entry-put nil "DEADLINE" (ht-get gtask "due")))
  (let ((status (ht-get gtask "status")))
    (org-entry-put nil "GTASKS-STATUS" status)
    (if (equal status "completed")
        (org-todo "DONE")))
  (if (ht-get gtask "completed")
      (org-entry-put nil "GTASKS-COMPLETED" (ht-get gtask "completed")))
  (if (ht-get gtask "deleted")
      (org-entry-put nil "GTASKS-DELETED" "true"))
  (if (ht-get gtask "hidden")
      (org-entry-put nil "GTASKS-HIDDEN" "true")) ; Read only parameter
  ;; TODO: links
)

(defun org-sync-gtasks--make-gtask-from-headline ()
  "Make gtask as a hash table from the headline properties."
  (let ((gtask     (ht-create))
        (title    (org-entry-get nil "ITEM"))
        (todo     (org-entry-get nil "TODO"))
        (deadline (org-entry-get nil "DEADLINE"))
        (notes    (org-entry-get nil "GTASKS-NOTES")))
    (ht-set! gtask "title" title)
    (if deadline
        (ht-set! gtask "due" (format-time-string "%Y-%m-%dT%H:%M:00.000Z"
                                                (date-to-time deadline))))
    (if notes
        (ht-set! gtask "notes" notes))
    (if (equal todo "DONE")
        (progn
          (ht-set! gtask "status" "completed")
          (org-entry-put nil "GTASKS-STATUS" "completed")))
    gtask))

;;; Entry point 1
(defun org-sync-gtasks-sync-at-point (&optional cache)
  "Synchronize GTasks and an Org todo headline at point.

This command Synchronizes the todo headlines at your cursor.
Make the headline as TODO if not, and create a new GTasks task item."
  (interactive)
  ;; Check major-mode
  (if (not (eq major-mode 'org-mode))
      (error "Please use this command in org-mode"))
  (let* ((tasklist-id (org-sync-gtasks--get-or-put-tasklist-id))
         (task-id    (org-entry-get nil "GTASKS-ID"))
         (etag       (org-entry-get nil "GTASKS-ETAG"))
         (gtask      (org-sync-gtasks--make-gtask-from-headline)))

    ;; Make this headline a todo item.
    (if (org-entry-get nil "TODO")
        nil
      (org-entry-put nil "TODO" "TODO") )

     ;; Update Org headline's properties.
     (org-sync-gtasks--update-todo-headline
       tasklist-id
      (cond
       ;; If the task doesn't have gtask-id,
       ((eq task-id nil)
        ;; Insert a new task to GTasks.
        (org-sync-gtasks--api-tasks-insert tasklist-id gtask))
       ;; If the task has gtask-id,
       (t
         ;; Push or Pull the task to GTask
         ;;   Compare etags. If etags are defferent, get the task from GTasks
         ;;   Otherwise, patch the task to GTasks
        (cond
         ;; Compare Org headline's etag and remote GTasks etag.
         ((equal
           etag
           (if (and cache (ht-get cache task-id))
               (ht-get (ht-get cache task-id) "etag")
              ;; TODO: do not use org-sync-gtasks--get-task-etag
              (org-sync-gtasks--get-task-etag tasklist-id task-id)))
           ;; If they are equal, push the task to GTasks by patch method.
          (org-sync-gtasks--api-tasks-patch tasklist-id task-id gtask))
         (t
           ;; Pull the task from GTask by get method.
           (if (and cache (ht-get cache task-id))
               (ht-get cache task-id)
             (org-sync-gtasks--api-tasks-get tasklist-id task-id)))))))))

(ert-deftest org-sync-gtasks-sync-at-point-test ()
  (org-sync-gtasks--test-with-org-buffer
     :target
     (org-sync-gtasks-sync-at-point)
     :list
     (
;;       (:input
;;        "* My New Task
;; "
;;        :output
;;        "\\* My New Task
;; :PROPERTIES:
;; :GTASKS-TASKLIST-ID: .*
;; :GTASKS-ID: .*
;; :GTASKS-ETAG: .*
;; :END:
;; ")
      (:input
       "* Important Task
SCHEDULED: <2022-03-23 Wed> DEADLINE: <2022-03-30 Tue>
:PROPERTIES:
:GTASKS-TASKLIST-ID: MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow
:GTASKS-ID: bmFJZ2pFbGhQUXU0OU1xYQ
:GTASKS-ETAG: \"FORCEUPDATE\"
:GTASKS-NOTES: This new notes text.
:END:
"
       :output
       "\\* Important Task
SCHEDULED: <2022-03-23 Wed> DEADLINE: <2022-03-30 Tue>
:PROPERTIES:
:GTASKS-TASKLIST-ID: MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow
:GTASKS-ID: bmFJZ2pFbGhQUXU0OU1xYQ
:GTASKS-ETAG: .*
:GTASKS-NOTES: This new notes text.
:END:
"))))
;;; Entry point 2
(defun org-sync-gtasks-sync ()
  "Synchronize GTasks and Org todo headlines.

Synchronize every todo headlines with GTASKS-ID property.
Also, pull other GTasks tasks as new headines."
  (interactive)
  ;; Check major-mode
  (if (not (eq major-mode 'org-mode))
      (error "Please use this command in org-mode"))
  ;; Make a list of GTASKS-ID by looking up all org TODO headlines in agenda.
  (let ((todos)
       (tasklist-id (org-sync-gtasks--default-tasklist-id))
       (table       (org-sync-gtasks--make-id-to-gtask-table)))
    (org-map-entries
     (lambda ()
       ;; Update todo headlines with valid GTASKS-ID.
       ;; TODO: do not update copmleted or deleted tasks.
       (let ((gtasks-id (org-entry-get nil "GTASKS-ID")))
         (when (and  gtasks-id
                     (not (equal gtasks-id "")))
           (message "GTasks update: %s" (org-entry-get nil "ITEM"))
           (org-sync-gtasks-sync-at-point table)
           (ht-remove! table gtasks-id)))) ; Remove this todo.
     "+TODO={.+}"
     'agenda)
    ;; Insert org headlines from rest of the table.
    (dolist (gtasks-id (ht-keys table))
      (message "GTasks insert: %s" (ht-get (ht-get table gtasks-id) "title"))
      (org-sync-gtasks--insert-todo-headline
       tasklist-id
       (ht-get table gtasks-id)))))
