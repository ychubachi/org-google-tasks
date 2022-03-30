;;; org-sync-gtasks-test.el --- Tests for org-sync-gtasks

(require 'org-sync-gtasks)
;;; For tests.
(require 'el-mock)
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
    (let ((r (mapcar (lambda (x)
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
                     list)))
      (push 'progn r))))

(ert-deftest org-sync-gtasks--test-with-org-buffer-test()
  "Test org-sync-gtasks--test-with-org-buffer macro for development purpose."
  (with-mock
    (stub org-id-new => "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
    (org-sync-gtasks--test-with-org-buffer
     :input
     "* headline
"
     :output
     "\\* headline
:PROPERTIES:
:ID:       .*
:END:
"
     :target
     (org-id-get-create) ;; this needs file related buffers.
     )))

;; Tests
(ert-deftest org-sync-gtasks--default-tasklist-id-test ()
  (with-mock
    (stub org-sync-gtasks--api-tasklists-list =>
          (ht ("items" `[,(ht ("id" "SOME-GTASKLIST-ID"))])))
    (let ((result (org-sync-gtasks--default-tasklist-id)))
      (should (equal result "SOME-GTASKLIST-ID")))))

(ert-deftest org-sync-gtasks--make-id2gtask-table-test ()
  (with-mock
    (stub org-sync-gtasks--api-tasklists-list =>
	  #s(hash-table test equal data
                        ("kind" "tasks#taskLists" "etag" "\"MjA0MzIwOTcyNw\"" "items" [#s(hash-table test equal data ("kind" "tasks#taskList" "id" "MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow" "etag" "\"LTE1Nzc5OTMyNDA\"" "title" "Tasklist" "updated" "2022-03-19T11:30:48.118Z" "selfLink" "https://www.googleapis.com/tasks/v1/users/@me/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow")) #s(hash-table test equal data ("kind" "tasks#taskList" "id" "MkhZNnRBVGE4eFk2UW1sdw" "etag" "\"LTE4NTg3MTY1ODQ\"" "title" "タスク" "updated" "2022-03-16T05:32:05.318Z" "selfLink" "https://www.googleapis.com/tasks/v1/users/@me/lists/MkhZNnRBVGE4eFk2UW1sdw")) #s(hash-table test equal data ("kind" "tasks#taskList" "id" "M21GQnJNQm84YXdCWVZRcw" "etag" "\"LTE1NzA3ODUwNzE\"" "title" "はひふへほ" "updated" "2022-03-19T13:30:56.845Z" "selfLink" "https://www.googleapis.com/tasks/v1/users/@me/lists/M21GQnJNQm84YXdCWVZRcw"))])))
    (stub org-sync-gtasks--api-tasks-list =>
	  #s(hash-table test equal data
			("kind" "tasks#tasks" "etag" "\"LTE1Nzc5OTMyNDA\"" "items" [#s(hash-table size 9 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#task" "id" "YWlqV0hsRV9lYVlQdkx5MQ" "etag" "\"LTIxMDQ5NTgyODI\"" "title" "Task2 with note" "updated" "2022-03-13T09:08:03.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/YWlqV0hsRV9lYVlQdkx5MQ" "position" "00000000000000000001" "notes" "This is note string.
Hello World!" "status" "needsAction")) #s(hash-table test equal data ("kind" "tasks#task" "id" "dlAzdXRlWDh2Z0dsck4xcQ" "etag" "\"MjA3MjIxMDY4Nw\"" "title" "Task 7 as sub-task" "updated" "2022-03-12T00:24:45.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/dlAzdXRlWDh2Z0dsck4xcQ" "parent" "aEQ1TjhNOGRiS3p6VmR4dw" "position" "00000000000000000000" "status" "needsAction")) #s(hash-table test equal data ("kind" "tasks#task" "id" "aEQ1TjhNOGRiS3p6VmR4dw" "etag" "\"MjA3MjE3NDc0OA\"" "title" "Task6 as parent" "updated" "2022-03-12T00:24:09.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/aEQ1TjhNOGRiS3p6VmR4dw" "position" "00000000000000000005" "status" "needsAction")) #s(hash-table test equal data ("kind" "tasks#task" "id" "Y1hxLXB0ZHJZb0x0Z3I0Mw" "etag" "\"MjA3MjE1ODc0MQ\"" "title" "Task5 with date time" "updated" "2022-03-12T00:23:53.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/Y1hxLXB0ZHJZb0x0Z3I0Mw" "position" "00000000000000000004" "status" "needsAction" "due" "2022-03-15T00:00:00.000Z")) #s(hash-table test equal data ("kind" "tasks#task" "id" "aUhGRlpRMXhqbmZrN1JsQQ" "etag" "\"MjA3MjE0NDc4NQ\"" "title" "Task4 with date (repeat)" "updated" "2022-03-12T00:23:38.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/aUhGRlpRMXhqbmZrN1JsQQ" "position" "00000000000000000003" "status" "needsAction" "due" "2022-03-15T00:00:00.000Z"))])))
    (let* ((table (org-sync-gtasks--make-id2gtask-table))
          (result (ht-keys table)))
      (should (equal
	       (format "%s" result)
               "(aUhGRlpRMXhqbmZrN1JsQQ Y1hxLXB0ZHJZb0x0Z3I0Mw aEQ1TjhNOGRiS3p6VmR4dw dlAzdXRlWDh2Z0dsck4xcQ YWlqV0hsRV9lYVlQdkx5MQ)"
               )))))

;; (ert-deftest org-sync-gtasks-sync-at-point-test ()
;;   (org-sync-gtasks--test-with-org-buffer
;;      :target
;;      (org-sync-gtasks-sync-at-point)
;;      :list
;;      (
;; ;;       (:input
;; ;;        "* My New Task
;; ;; "
;; ;;        :output
;; ;;        "\\* My New Task
;; ;; :PROPERTIES:
;; ;; :GTASKS-TASKLIST-ID: .*
;; ;; :GTASKS-ID: .*
;; ;; :GTASKS-ETAG: .*
;; ;; :END:
;; ;; ")
;;       (:input
;;        "* Important Task
;; SCHEDULED: <2022-03-23 Wed> DEADLINE: <2022-03-30 Tue>
;; :PROPERTIES:
;; :GTASKS-TASKLIST-ID: MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow
;; :GTASKS-ID: bmFJZ2pFbGhQUXU0OU1xYQ
;; :GTASKS-ETAG: \"FORCEUPDATE\"
;; :GTASKS-NOTES: This new notes text.
;; :END:
;; "
;;        :output
;;        "\\* Important Task
;; SCHEDULED: <2022-03-23 Wed> DEADLINE: <2022-03-30 Tue>
;; :PROPERTIES:
;; :GTASKS-TASKLIST-ID: MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow
;; :GTASKS-ID: bmFJZ2pFbGhQUXU0OU1xYQ
;; :GTASKS-ETAG: .*
;; :GTASKS-NOTES: This new notes text.
;; :END:
;; "))))

(ert-deftest org-sync-gtasks--get-or-default-id-test ()
  (with-mock
    (stub org-sync-gtasks--default-tasklist-id => "NEW-GTASKS-TASKLIST-ID")
    (org-sync-gtasks--test-with-org-buffer
     :target
     (org-sync-gtasks--get-or-default-tasklist-id)
     :list
     ((:input
       "* headline
"
       :output
       "* headline
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

(ert-deftest org-sync-gtasks--update-todo-headline-test ()
  (with-mock
    (org-sync-gtasks--test-with-org-buffer
     :target
     (let ((gtask (ht ("title" "TITLE")
                      ("id" "TEST-TASK-ID")
                      ("etag" "TEST-ETAG")
                      ("notes" "TEST-NOTES")
                      ("status" "needsAction"))))
       (org-sync-gtasks--update-todo-headline "TEST-TASKLIST-ID" gtask))
     ;; TODO: Test status equals completed case
     :list
     ((:input
       "* dummy
"
       :output
       "\\* TODO TITLE
:PROPERTIES:
:GTASKS-TASKLIST-ID: TEST-TASKLIST-ID
:GTASKS-ID: TEST-TASK-ID
:GTASKS-ETAG: TEST-ETAG
:GTASKS-NOTES: TEST-NOTES
:GTASKS-STATUS: needsAction
:END:
")))))

(ert-deftest org-sync-gtasks--insert-todo-headline-test ()
  (with-mock
    (org-sync-gtasks--test-with-org-buffer
     :target
     (let ((gtask (ht ("title" "TITLE")
                      ("id" "TEST-TASK-ID")
                      ("etag" "TEST-ETAG"))))
       (org-sync-gtasks--insert-todo-headline "TEST-TASKLIST-ID" gtask))
     :list
     ((:input
       ""
       :output
       "\\* TODO TITLE
:PROPERTIES:
:GTASKS-TASKLIST-ID: TEST-TASKLIST-ID
:GTASKS-ID: TEST-TASK-ID
:GTASKS-ETAG: TEST-ETAG
:END:
")
      (:input
       "* dummy
"
       :output
       "\\ *dummy
\\* TODO TITLE
:PROPERTIES:
:GTASKS-TASKLIST-ID: TEST-TASKLIST-ID
:GTASKS-ID: TEST-TASK-ID
:GTASKS-ETAG: TEST-ETAG
:END:
")))))

;; TODO: We need another framework...
(ert-deftest org-sync-gtasks--make-gtask-from-headline-test ()
  (with-mock
    (org-sync-gtasks--test-with-org-buffer
     :target
     (org-sync-gtasks--make-gtask-from-headline) ;; TODO: Need to check its return value.
     :list
     ((:input
       "* TODO TITLE
:PROPERTIES:
:GTASKS-TASKLIST-ID: TEST-TASKLIST-ID
:GTASKS-ID: TEST-TASK-ID
:GTASKS-ETAG: TEST-ETAG
:END:
"
       :output
       "")
      ))))

;;; org-sync-gtasks-test.el ends here
