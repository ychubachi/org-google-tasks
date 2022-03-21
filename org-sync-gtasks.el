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
REQUEST-DATA is any emacs lisp object that json-serialize understands."
  `(let* ((response-buffer (oauth2-url-retrieve-synchronously
			 (org-sync-gtasks--token)
			 ,url
			 ,request-method
			 (if ,request-data
			     (encode-coding-string (json-serialize ,request-data) 'utf-8))))
	  (response (org-sync-gtasks--parse-http-response response-buffer))
          ;; TODO: Error handling
	  (body (decode-coding-string (plist-get response :body) 'utf-8)))
     (json-parse-string body)))

;;; Google Tasks APIs for tasklists.
(defun org-sync-gtasks--api-tasklists-list ()
  "Creates a new task list and adds it to the authenticated user's task lists.

See URL https://developers.google.com/tasks/reference/rest/v1/tasklists/list"
  (org-sync-gtasks--api "https://tasks.googleapis.com/tasks/v1/users/@me/lists"
	  "GET"))

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

(defun org-sync-gtasks--api-tasks-list (tasklist)
  "Returns all tasks in the specified task list.

See URL https://developers.google.com/tasks/reference/rest/v1/tasks/list"
  (org-sync-gtasks--api (format
	   "https://tasks.googleapis.com/tasks/v1/lists/%s/tasks" tasklist)
	  "GET"))

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

;;; Tasklists interface
(defun org-sync-gtasks--default-tasklist ()
  "This gets the defult tasklist. GTasks has at least one tasklist."
  (let ((tasklists (org-sync-gtasks--api-tasklists-list)))
    (aref (ht-get tasklists "items") 0)))

(ert-deftest org-sync-gtasks--default-tasklist-test ()
  (with-mock
    (stub org-sync-gtasks--default-tasklist => (ht ("kind" "tasks#taskList")))
    (let ((result (org-sync-gtasks--default-tasklist)))
      (should (equal (ht-get result "kind") "tasks#taskList")))))

;;;
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
(defun org-sync-gtasks--get-tasklist-id ()
  (let ((gtasklist-id (org-entry-get nil "GTASKLIST-ID")))
    (if (eq gtasklist-id nil)
        (org-entry-put nil "GTASKLIST-ID" "NEW-GTASKLIST-ID"))))

(ert-deftest org-sync-gtasks--get-tasklist-id-test ()
  (org-sync-gtasks--test-with-org-buffer
   :target
   (org-sync-gtasks--get-tasklist-id)
   :list
   ((:input
     "* headline
"
     :output
     "* headline
:PROPERTIES:
:GTASKLIST-ID: NEW-GTASKLIST-ID
:END:
")
    (:input
     "* headline
:PROPERTIES:
:GTASKLIST-ID: SOME-GTASKLIST-ID
:END:
"
     :output
     "* headline
:PROPERTIES:
:GTASKLIST-ID: SOME-GTASKLIST-ID
:END:
"))))
