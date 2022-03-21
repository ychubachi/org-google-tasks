;;; -*- lexical-binding: t -*-

;; call
(require 'ht)
(require 'oauth2)

;;; For tests.
(require 'el-mock)

(defcustom org-sync-gtasks--gtasks-client-secret-json ""
  "JSON file location to client secret."
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

;;; Macro for Google Tasks API.
(defmacro org-sync-gtasks--api (url &optional request-method request-data)
  "This is a macro for REST API request.
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
         (error "GTask API error: %s" reason))
     :status status :reason reason
     ;; Error in HTTP response body
     (setq result (json-parse-string body))
     ;; Errors in JSON response
     (let ((error-data (ht-get result "error")))
       (if error-data
           (error "GTask API error: %s" (ht-get error-data "message"))))
     result))

;;; Google Tasks APIs for tasklists.
(defun org-sync-gtasks--api-tasklists-list ()
  "Creates a new task list and adds it to the authenticated user's task lists.

See URL https://developers.google.com/tasks/reference/rest/v1/tasklists/list"
  (org-sync-gtasks--api "https://tasks.googleapis.com/tasks/v1/users/@me/lists"
	                "GET"))

;; Test case:
;; (org-sync-gtasks--api-tasklists-list)
;; => #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#taskLists" "etag" "\"LTczMzUyMzE3Nw\"" "items" [#s(hash-table size 6 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#taskList" "id" "MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow" "etag" "\"LTE0MDA4ODA3NTY\"" "title" "Tasklist" "updated" "2022-03-21T12:42:40.530Z" "selfLink" "https://www.googleapis.com/tasks/v1/users/@me/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow"))]))

(defun org-sync-gtasks--api-tasklists-insert (tasklist)
  "Creates a new task list and adds it to the authenticated user's task lists.

TASKLIST is a tasklist object. This returns response in JSON strings.
See URL https://developers.google.com/tasks/reference/rest/v1/tasklists/insert

Usage:
  (org-sync-gtasks--api-tasklists-insert '(:title \"My Tasklist\"))
  (org-sync-gtasks--api-tasklists-insert '((title . \"My Tasklist\")))
"
  (org-sync-gtasks--api "https://tasks.googleapis.com/tasks/v1/users/@me/lists"
	  "POST"
	  tasklist))

;; (org-sync-gtasks--api-tasklists-list)
;; (org-sync-gtasks--api-tasklists-insert '(:title "はひふへほ"))
;; (org-sync-gtasks--api-tasklists-insert '((title . "はひふ")))
;; (org-sync-gtasks--api "https://tasks.googleapis.com/tasks/v1/users/@me/lists" "POST" '(:title "さしすせそ"))

;;; Google Tasks APIs for tasks.
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

(defun org-sync-gtasks--api-tasks-insert (tasklist-id task) ; TODO: task -> tasks
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

;;; TODO
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

;;; Utils
(defun org-sync-gtasks--create-id-table ()
  "Create a hash table for looking up tasklist or task items by the id."
  (let* ((table (ht-create))
	 (tasklists (org-sync-gtasks--api-tasklists-list))
	 (tasklists-items (ht-get tasklists "items")))
    (dolist (tasklist (cl-coerce tasklists-items 'list))
      (let* ((tasklist-id (ht-get tasklist "id")))
		;; Set the tasklist to the table.
		(ht-set! table tasklist-id tasklist)
		;; Get the tasks under the tasklist.
		(let* ((tasks (org-sync-gtasks--api-tasks-list tasklist-id))
		       (tasks-items (ht-get tasks "items")))
		  (message "tasks:\n%s\nend:" tasks)
		  (dolist (task (cl-coerce tasks-items 'list))
		    (let ((task-id (ht-get task "id")))
		       ;; Set the task to the table.
		       (ht-set! table task-id task))))))
    table))

(ert-deftest org-sync-gtasks--create-id-table-test ()
  (with-mock
    (stub org-sync-gtasks--api-tasklists-list =>
	  #s(hash-table test equal data
                        ("kind" "tasks#taskLists" "etag" "\"MjA0MzIwOTcyNw\"" "items" [#s(hash-table test equal data ("kind" "tasks#taskList" "id" "MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow" "etag" "\"LTE1Nzc5OTMyNDA\"" "title" "Tasklist" "updated" "2022-03-19T11:30:48.118Z" "selfLink" "https://www.googleapis.com/tasks/v1/users/@me/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow")) #s(hash-table test equal data ("kind" "tasks#taskList" "id" "MkhZNnRBVGE4eFk2UW1sdw" "etag" "\"LTE4NTg3MTY1ODQ\"" "title" "タスク" "updated" "2022-03-16T05:32:05.318Z" "selfLink" "https://www.googleapis.com/tasks/v1/users/@me/lists/MkhZNnRBVGE4eFk2UW1sdw")) #s(hash-table test equal data ("kind" "tasks#taskList" "id" "M21GQnJNQm84YXdCWVZRcw" "etag" "\"LTE1NzA3ODUwNzE\"" "title" "はひふへほ" "updated" "2022-03-19T13:30:56.845Z" "selfLink" "https://www.googleapis.com/tasks/v1/users/@me/lists/M21GQnJNQm84YXdCWVZRcw"))])))
    (stub org-sync-gtasks--api-tasks-list =>
	  #s(hash-table test equal data
			("kind" "tasks#tasks" "etag" "\"LTE1Nzc5OTMyNDA\"" "items" [#s(hash-table size 9 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#task" "id" "YWlqV0hsRV9lYVlQdkx5MQ" "etag" "\"LTIxMDQ5NTgyODI\"" "title" "Task2 with note" "updated" "2022-03-13T09:08:03.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/YWlqV0hsRV9lYVlQdkx5MQ" "position" "00000000000000000001" "notes" "This is note string.
Hello World!" "status" "needsAction")) #s(hash-table test equal data ("kind" "tasks#task" "id" "dlAzdXRlWDh2Z0dsck4xcQ" "etag" "\"MjA3MjIxMDY4Nw\"" "title" "Task 7 as sub-task" "updated" "2022-03-12T00:24:45.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/dlAzdXRlWDh2Z0dsck4xcQ" "parent" "aEQ1TjhNOGRiS3p6VmR4dw" "position" "00000000000000000000" "status" "needsAction")) #s(hash-table test equal data ("kind" "tasks#task" "id" "aEQ1TjhNOGRiS3p6VmR4dw" "etag" "\"MjA3MjE3NDc0OA\"" "title" "Task6 as parent" "updated" "2022-03-12T00:24:09.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/aEQ1TjhNOGRiS3p6VmR4dw" "position" "00000000000000000005" "status" "needsAction")) #s(hash-table test equal data ("kind" "tasks#task" "id" "Y1hxLXB0ZHJZb0x0Z3I0Mw" "etag" "\"MjA3MjE1ODc0MQ\"" "title" "Task5 with date time" "updated" "2022-03-12T00:23:53.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/Y1hxLXB0ZHJZb0x0Z3I0Mw" "position" "00000000000000000004" "status" "needsAction" "due" "2022-03-15T00:00:00.000Z")) #s(hash-table test equal data ("kind" "tasks#task" "id" "aUhGRlpRMXhqbmZrN1JsQQ" "etag" "\"MjA3MjE0NDc4NQ\"" "title" "Task4 with date (repeat)" "updated" "2022-03-12T00:23:38.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/aUhGRlpRMXhqbmZrN1JsQQ" "position" "00000000000000000003" "status" "needsAction" "due" "2022-03-15T00:00:00.000Z"))])))
    (let ((result))
      (setq result (ht-keys (org-sync-gtasks--create-id-table)))
      (should (equal
	       (format "%s" result)
	       "(M21GQnJNQm84YXdCWVZRcw MkhZNnRBVGE4eFk2UW1sdw aUhGRlpRMXhqbmZrN1JsQQ Y1hxLXB0ZHJZb0x0Z3I0Mw aEQ1TjhNOGRiS3p6VmR4dw dlAzdXRlWDh2Z0dsck4xcQ YWlqV0hsRV9lYVlQdkx5MQ MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow)")))))

;;; Tasklist
(defun org-sync-gtasks--default-tasklist ()
  "This gets the defult tasklist. GTasks has at least one tasklist."
  (let ((tasklists (org-sync-gtasks--api-tasklists-list)))
    (aref (ht-get tasklists "items") 0)))

(ert-deftest org-sync-gtasks--default-tasklist-test ()
  (with-mock
    (stub org-sync-gtasks--default-tasklist => (ht ("kind" "tasks#taskList")))
    (let ((result (org-sync-gtasks--default-tasklist)))
      (should (equal (ht-get result "kind") "tasks#taskList")))))

(defun org-sync-gtasks--default-tasklist-id ()
  "This gets the defult tasklist ID."
  (ht-get (org-sync-gtasks--default-tasklist) "id"))

(ert-deftest org-sync-gtasks--default-tasklist-id-test ()
  (with-mock
    (stub org-sync-gtasks--default-tasklist => (ht ("id" "SOME-GTASKLIST-ID")))
    (let ((result (org-sync-gtasks--default-tasklist-id)))
      (should (equal result "SOME-GTASKLIST-ID")))))

;;; Task TODO
(defun org-sync-gtasks--insert-task (tasklist-id task)
  ;; returns new task id
  (ht-get (org-sync-gtasks--api-tasks-insert tasklist-id task) "id"))

(defun org-sync-gtasks--patch-task (tasklist-id task-id task)
  ;; returns new task id
  (ht-get (org-sync-gtasks--api-tasks-patch tasklist-id task-id task) "id"))

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
                           (should (equal result ,output))))
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
(defun org-sync-gtasks--get-or-create-tasklist-id ()
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
     (org-sync-gtasks--get-or-create-tasklist-id)
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
;;; Entry point
(defun org-sync-gtasks-sync-at-point ()
  "Sync gtasks at point."
  (interactive)
  (let* ((tasklist-id
          (org-sync-gtasks--get-or-create-tasklist-id))
        (task-id (org-entry-get nil "GTASKS-TASK-ID"))
        ;; TODO: ETAG
        (title (org-entry-get nil "ITEM"))
        (deadline (org-entry-get nil "DEADLINE"))
        (notes (org-entry-get nil "GTASKS-NOTES"))
        ;; Create a task object
        (task))

    (message "tasklist-id=%s" tasklist-id)
    (message "tasklist-id=%s" task-id)

    ;; Get task's properties from the headline.
    (if deadline
        (setq task
              (append (list :due
                            (format-time-string "%Y-%m-%dT%H:%M:00.000Z"
                                                (date-to-time deadline)))
                      task)))
    (if notes
        (setq task (append (list :notes notes) task)))
    (setq task (append (list :title title) task))

    ;; if it doesn't have task-id, insert a new task and put its id to the headline.
    (if (eq task-id nil)
        (progn
          ;; Create a new task.
          (setq task-id (org-sync-gtasks--insert-task tasklist-id task))
          (org-entry-put nil "GTASKS-TASK-ID" task-id))
      (progn
        ;; TODO: Compare ETAG
        ;; Update an existting task.
        (org-sync-gtasks--patch-task tasklist-id task-id task)))))


;; (org-sync-gtasks--api-tasks-patch "MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow"
;;                                   "bmFJZ2pFbGhQUXU0OU1xYQ"
;;                                   '(:title "Important Task"))

(ert-deftest org-sync-gtasks-sync-at-point-test ()
  (org-sync-gtasks--test-with-org-buffer
     :target
     (org-sync-gtasks-sync-at-point)
     :list
     (
      (:input
       "* My New Task
"
       :output
       "* My New Task
:PROPERTIES:
:GTASKS-TASKLIST-ID: MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow
:GTASKS-TASK-ID: SOME-GTASKS-TASK-ID
:END:
")
      (:input
       "* More Important Task
SCHEDULED: <2022-03-23 Wed> DEADLINE: <2022-03-30 Tue>
:PROPERTIES:
:GTASKS-TASKLIST-ID: MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow
:GTASKS-TASK-ID: bmFJZ2pFbGhQUXU0OU1xYQ
:GTASKS-NOTES: This new notes text.
:END:
"
       :output
       "* More Important Task
SCHEDULED: <2022-03-23 Wed> DEADLINE: <2022-03-30 Tue>
:PROPERTIES:
:GTASKS-TASKLIST-ID: MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow
:GTASKS-TASK-ID: bmFJZ2pFbGhQUXU0OU1xYQ
:GTASKS-NOTES: This new notes text.
:END:
")
;;       (:input
;;        "* My Task
;; "
;;        :output
;;        "* My Task
;; :PROPERTIES:
;; :GTASKS-TASKLIST-ID: SOME-GTASKS-TASKLIST-ID
;; :GTASKS-TASK-ID: SOME-GTASKS-TASK-ID
;; :END:
      ;; ")
      ))
  )
