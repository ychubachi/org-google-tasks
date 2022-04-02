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
        (list   (plist-get rest :list)))
    ;; If we have only one example pair, make them to list.
    (if (eq list nil)
      (let ((input  (plist-get rest :input))
            (output (plist-get rest :output)))
        (setq list (list (list :input input :output output)))))
    ;; Iterate each example pair list.
    (let ((r (mapcar (lambda (x)
                      (let ((input  (plist-get x :input))
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
	                     (setq result-string
                                   (substring-no-properties (buffer-string)))
	                     (set-buffer-modified-p nil)
	                     (kill-buffer (current-buffer)))
                           (should (string-match ,output result-string))))
                      )
                     list)))
      (push 'progn r))))

(ert-deftest org-sync-gtasks--test-with-org-buffer-test ()
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

(ert-deftest org-sync-gtasks--test-with-org-buffer-test/error ()
  "Test on error."
  (org-sync-gtasks--test-with-org-buffer
   :input
   "* headline
"
   :target
   (error "test")
   :output
   "\\* headline
"))

;; Tests
(ert-deftest org-sync-gtasks--default-tasklist-id-test ()
  (with-mock
    (stub org-sync-gtasks--api-tasklists-list =>
          (ht ("items" `[,(ht ("id" "SOME-GTASKLIST-ID"))])))
    (let ((result (org-sync-gtasks--default-tasklist-id)))
      (should (equal result "SOME-GTASKLIST-ID")))))

(ert-deftest org-sync-gtasks--make-tasklist-cache-test ()
  (with-mock
    (stub org-sync-gtasks--api-tasks-list =>
	  #s(hash-table test equal data
			("kind" "tasks#tasks" "etag" "\"LTE1Nzc5OTMyNDA\"" "items" [#s(hash-table size 9 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("kind" "tasks#task" "id" "YWlqV0hsRV9lYVlQdkx5MQ" "etag" "\"LTIxMDQ5NTgyODI\"" "title" "Task2 with note" "updated" "2022-03-13T09:08:03.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/YWlqV0hsRV9lYVlQdkx5MQ" "position" "00000000000000000001" "notes" "This is note string.
Hello World!" "status" "needsAction")) #s(hash-table test equal data ("kind" "tasks#task" "id" "dlAzdXRlWDh2Z0dsck4xcQ" "etag" "\"MjA3MjIxMDY4Nw\"" "title" "Task 7 as sub-task" "updated" "2022-03-12T00:24:45.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/dlAzdXRlWDh2Z0dsck4xcQ" "parent" "aEQ1TjhNOGRiS3p6VmR4dw" "position" "00000000000000000000" "status" "needsAction")) #s(hash-table test equal data ("kind" "tasks#task" "id" "aEQ1TjhNOGRiS3p6VmR4dw" "etag" "\"MjA3MjE3NDc0OA\"" "title" "Task6 as parent" "updated" "2022-03-12T00:24:09.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/aEQ1TjhNOGRiS3p6VmR4dw" "position" "00000000000000000005" "status" "needsAction")) #s(hash-table test equal data ("kind" "tasks#task" "id" "Y1hxLXB0ZHJZb0x0Z3I0Mw" "etag" "\"MjA3MjE1ODc0MQ\"" "title" "Task5 with date time" "updated" "2022-03-12T00:23:53.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/Y1hxLXB0ZHJZb0x0Z3I0Mw" "position" "00000000000000000004" "status" "needsAction" "due" "2022-03-15T00:00:00.000Z")) #s(hash-table test equal data ("kind" "tasks#task" "id" "aUhGRlpRMXhqbmZrN1JsQQ" "etag" "\"MjA3MjE0NDc4NQ\"" "title" "Task4 with date (repeat)" "updated" "2022-03-12T00:23:38.000Z" "selfLink" "https://www.googleapis.com/tasks/v1/lists/MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow/tasks/aUhGRlpRMXhqbmZrN1JsQQ" "position" "00000000000000000003" "status" "needsAction" "due" "2022-03-15T00:00:00.000Z"))])))
    (let* ((table (org-sync-gtasks--make-tasklist-cache "TASKLIST-ID"))
          (result (ht-keys table)))
      (should (equal
	       (format "%s" result)
               "(aUhGRlpRMXhqbmZrN1JsQQ Y1hxLXB0ZHJZb0x0Z3I0Mw aEQ1TjhNOGRiS3p6VmR4dw dlAzdXRlWDh2Z0dsck4xcQ YWlqV0hsRV9lYVlQdkx5MQ)"
               )))))

;; (ert-deftest org-sync-gtasks-at-point-test ()
;;   (org-sync-gtasks--test-with-org-buffer
;;      :target
;;      (org-sync-gtasks-at-point)
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
                      ("parent" "TEST-PARENT")
                      ("notes" "TEST-NOTES")
                      ("status" "needsAction")
                      ("due" "2022-04-01T13:24:25.000Z")
                      ("completed" "TEST-COMPLETED")
                      ("deleted" "false")
                      ("hidden" "false"))))
       (org-sync-gtasks--update-todo-headline "TEST-TASKLIST-ID" gtask))
     ;; TODO: Test status equals completed case
     :list
     ((:input
       "* dummy
"
       :output
       "\\* TODO TITLE
DEADLINE: .*
:PROPERTIES:
:GTASKS-TASKLIST-ID: .*
:GTASKS-ID: .*
:GTASKS-ETAG: .*
:GTASKS-PARENT: .*
:GTASKS-NOTES: .*
:GTASKS-STATUS: .*
:GTASKS-COMPLETED: .*
:GTASKS-DELETED: .*
:GTASKS-HIDDEN: .*
:END:
")))))

(ert-deftest org-sync-gtasks--insert-todo-headline-test ()
  (org-sync-gtasks--test-with-org-buffer
   :target
   (let ((gtask (ht ("title" "TITLE")
                    ("id"    "TEST-TASK-ID")
                    ("etag"  "TEST-ETAG"))))
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
"))))

;; TODO: We need another framework...
(ert-deftest org-sync-gtasks--make-task-from-headline-test ()
  (with-mock
    (org-sync-gtasks--test-with-org-buffer
     :target
     (org-sync-gtasks--make-task-from-headline) ;; TODO: Need to check its return value.
     :list
     ((:input
       "* TODO TITLE
DEADLINE: <2022-04-01 Fri>
:PROPERTIES:
:GTASKS-TASKLIST-ID: TEST-TASKLIST-ID
:GTASKS-ID: TEST-TASK-ID
:GTASKS-ETAG: TEST-ETAG
:GTASKS-STATUS: TEST-STATUS
:GTASKS-NOTES: TEST-NOTES
:END:
"
       :output
       "\\* TODO TITLE
DEADLINE: <2022-04-01 Fri>
:PROPERTIES:
:GTASKS-TASKLIST-ID: TEST-TASKLIST-ID
:GTASKS-ID: TEST-TASK-ID
:GTASKS-ETAG: TEST-ETAG
:GTASKS-STATUS: TEST-STATUS
:GTASKS-NOTES: TEST-NOTES
:END:
")))))

(ert-deftest org-sync-gtasks--get-gtask-from-cache-or-api ()
  ;; Case 1
  (with-mock
   (stub org-sync-gtasks--api-tasks-get =>
         (ht ("title" "title of task")))
   (should (equal
            (ht-get
             (org-sync-gtasks--get-gtask-from-cache-or-api
              "tasklist-id" "task-id" nil)
             "title" )
            "title of task")))
  ;; Case 2
  (let ((cache (ht ("task-id"
                    (ht ("title" "title of task"))))))
    (should (equal
            (ht-get
             (org-sync-gtasks--get-gtask-from-cache-or-api
              "tasklist-id" "task-id" cache)
             "title" )
            "title of task"))))


(ert-deftest org-sync-gtasks--headline-modified-p-test/same-title ()
    "The headline is same"
  (org-sync-gtasks--test-with-org-buffer
   :input
   "* TODO TITLE
:PROPERTIES:
:GTASKS-STATUS: needsAction
:END:
"
   :target
   (should (eq (org-sync-gtasks--headline-modified-p
                (ht ("title" "TITLE") ("status" "needsAction")))
               nil))
   :output
   ""))

(ert-deftest org-sync-gtasks--headline-modified-p-test/modified-title ()
    "The headline is modified"
  (org-sync-gtasks--test-with-org-buffer
   :input
   "* TODO NEW TITLE
:PROPERTIES:
:GTASKS-STATUS: needsAction
:END:
"
   :target
   (should (eq (org-sync-gtasks--headline-modified-p
                (ht ("title" "TITLE") ("status" "needsAction")))
               t))
   :output
   ""))

(ert-deftest org-sync-gtasks--headline-modified-p-test/same-todo-status ()
    "Is the headline modified?"
  (org-sync-gtasks--test-with-org-buffer
   :input
   "* TODO TITLE
"
   :target
   (should (eq (org-sync-gtasks--headline-modified-p
                (ht ("title" "TITLE") ("status" "needsAction")))
               nil)) ; modified
   :output
   ""))

(ert-deftest org-sync-gtasks--headline-modified-p-test/not-same-todo-status ()
    "Is the headline modified?"
  (org-sync-gtasks--test-with-org-buffer
   :input
   "* TODO TITLE
"
   :target
   (should (eq (org-sync-gtasks--headline-modified-p
                (ht ("title" "TITLE") ("status" "completed")))
               t))
   :output
   ""))

(ert-deftest org-sync-gtasks--headline-modified-p-test/same-done-status ()
    "Is the headline modified?"
  (org-sync-gtasks--test-with-org-buffer
   :input
   "* DONE TITLE
"
   :target
   (should (eq (org-sync-gtasks--headline-modified-p
                (ht ("title" "TITLE") ("status" "completed")))
               nil))
   :output
   ""))

(ert-deftest org-sync-gtasks--headline-modified-p-test/not-same-done-status ()
    "Is the headline modified?"
  (org-sync-gtasks--test-with-org-buffer
   :input
   "* DONE TITLE
"
   :target
   (should (eq (org-sync-gtasks--headline-modified-p
                (ht ("title" "TITLE") ("status" "needsAction")))
               t))
   :output
   ""))

(ert-deftest org-sync-gtasks--headline-modified-p-test/same-deadline ()
    "Is the headline modified?"
  (org-sync-gtasks--test-with-org-buffer
   :input
   "* TODO TITLE
DEADLINE: <2022-04-01>
"
   :target
   (should (eq (org-sync-gtasks--headline-modified-p
                (ht ("title" "TITLE") ("status" "needsAction")
                    ("due" "2022-04-01T13:24:25.000Z")))
               nil))
   :output
   ""))

(ert-deftest org-sync-gtasks--headline-modified-p-test/not-same-deadline ()
    "Is the headline modified?"
  (org-sync-gtasks--test-with-org-buffer
   :input
   "* TODO TITLE
DEADLINE: <2022-04-02>
"
   :target
   (should (eq (org-sync-gtasks--headline-modified-p
                (ht ("title" "TITLE") ("status" "needsAction")
                    ("due" "2022-04-01T13:24:25.000Z")))
               t))
   :output
   ""))

(ert-deftest org-sync-gtasks-at-point-test/check-org-mode ()
  "Not in org-mode"
  (should-error (org-sync-gtasks-at-point)))

(ert-deftest org-sync-gtasks-at-point-test/no-title ()
  "No title"
  (org-sync-gtasks--test-with-org-buffer
   :target
   (should-error (org-sync-gtasks-at-point))
   :list
   ((:input
     ""
     :output
     ""))))


(ert-deftest org-sync-gtasks-at-point-test/insert ()
  "Insert a new task to Google Tasks."
  (org-sync-gtasks--test-with-org-buffer
   :input
   "* Title
"
   :target
   (with-mock
    (stub org-sync-gtasks--get-or-default-tasklist-id =>
          "TASKLIST-ID")
    (stub org-sync-gtasks--api-tasks-insert =>
          (ht ("title" "Title")
              ("id" "TASK-ID")
              ("etag" "ETAG")
              ("status" "needsAction")))
    (org-sync-gtasks-at-point))
   :output
   "\\* TODO Title
:PROPERTIES:
:GTASKS-TASKLIST-ID: TASKLIST-ID
:GTASKS-ID: TASK-ID
:GTASKS-ETAG: ETAG
:GTASKS-STATUS: needsAction
:END:
"))

(ert-deftest org-sync-gtasks-at-point-test/keep ()
  "Do nothing"
  (org-sync-gtasks--test-with-org-buffer
   :input
   "* TODO Title
:PROPERTIES:
:GTASKS-TASKLIST-ID: TASKLIST-ID
:GTASKS-ID: TASK-ID
:GTASKS-ETAG: ETAG
:GTASKS-STATUS: needsAction
:END:
"
   :target
   (with-mock
    (stub org-sync-gtasks--get-or-default-tasklist-id =>
          "TASKLIST-ID")
    ;; (stub org-sync-gtasks--default-tasklist-id => "HOGEE")
    (stub org-sync-gtasks--get-gtask-from-cache-or-api =>
          (ht ("title"  "Title")
              ("id"     "TASK-ID")
              ("etag"   "ETAG")
              ("status" "needsAction")))
    (org-sync-gtasks-at-point))
   :output
   "\\* TODO Title
:PROPERTIES:
:GTASKS-TASKLIST-ID: TASKLIST-ID
:GTASKS-ID: .*
:GTASKS-ETAG: .*
:GTASKS-STATUS: needsAction
:END:
"))

(ert-deftest org-sync-gtasks-at-point-test/patch ()
  "Patch"
  (org-sync-gtasks--test-with-org-buffer
   :input
   "* TODO New Title
DEADLINE: <2022-04-01>
:PROPERTIES:
:GTASKS-TASKLIST-ID: TASKLIST-ID
:GTASKS-ID: TASK-ID
:GTASKS-ETAG: ETAG
:GTASKS-STATUS: needsAction
:END:
"
   :target
   (with-mock
    (stub org-sync-gtasks--get-or-default-tasklist-id =>
          "TASKLIST-ID")
    ;; (stub org-sync-gtasks--default-tasklist-id => "HOGEE")
    (stub org-sync-gtasks--get-gtask-from-cache-or-api =>
          (ht ("title"  "Title")
              ("id"     "TASK-ID")
              ("etag"   "ETAG")
              ("status" "needsAction")))
    (stub org-sync-gtasks--api-tasks-patch =>
          (ht ("title"  "Title")
              ("id"     "TASK-ID")
              ("etag"   "ETAG")
              ("status" "needsAction")
              ("due"    "2022-04-01")))
    (org-sync-gtasks-at-point))
   :output
   "\\* TODO Title
DEADLINE: <2022-04-01 Fri>
:PROPERTIES:
:GTASKS-TASKLIST-ID: TASKLIST-ID
:GTASKS-ID: .*
:GTASKS-ETAG: .*
:GTASKS-STATUS: needsAction
:END:
"))

(ert-deftest org-sync-gtasks-at-point-test/get ()
  "Update"
  (org-sync-gtasks--test-with-org-buffer
   :input
   "* TODO Title
DEADLINE: <2022-04-01>
:PROPERTIES:
:GTASKS-TASKLIST-ID: TASKLIST-ID
:GTASKS-ID: TASK-ID
:GTASKS-ETAG: NEW-ETAG
:GTASKS-STATUS: needsAction
:END:
"
   :target
   (with-mock
    (stub org-sync-gtasks--get-or-default-tasklist-id =>
          "TASKLIST-ID")
    ;; (stub org-sync-gtasks--default-tasklist-id => "HOGEE")
    (stub org-sync-gtasks--get-gtask-from-cache-or-api =>
          (ht ("title"  "Title")
              ("id"     "TASK-ID")
              ("etag"   "ETAG")
              ("status" "needsAction")
              ("due"    "2022-04-16T00:00:00.000Z")))
    (org-sync-gtasks-at-point))
   :output
   "\\* TODO Title
DEADLINE: <2022-04-16 Sat>
:PROPERTIES:
:GTASKS-TASKLIST-ID: TASKLIST-ID
:GTASKS-ID: .*
:GTASKS-ETAG: .*
:GTASKS-STATUS: needsAction
:END:
"))

(ert-deftest org-sync-gtasks-agenda-test/check-org-mode ()
  "Not in org-mode"
  (should-error (org-sync-gtasks-agenda)))

(ert-deftest org-sync-gtasks-agenda-test/done-and-completed ()
  "Do nothing."
  (org-sync-gtasks--test-with-org-buffer
   :input
   "* DONE Title
:PROPERTIES:
:GTASKS-TASKLIST-ID: TASKLIST-ID
:GTASKS-ID: TASK-ID
:GTASKS-ETAG: ETAG
:GTASKS-STATUS: completed
:END:
"
   :target
   (with-mock
    (stub org-sync-gtasks--get-or-default-tasklist-id =>
          "TASKLIST-ID")
    (org-sync-gtasks-at-point))
   :output
   "\\* DONE Title
:PROPERTIES:
:GTASKS-TASKLIST-ID: TASKLIST-ID
:GTASKS-ID: TASK-ID
:GTASKS-ETAG: ETAG
:GTASKS-STATUS: completed
:END:
"))

(ert-deftest org-sync-gtasks-agenda-test/done-and-needsAction ()
  "If the headline is DONE but its status is needsAction, need to patch."
  (org-sync-gtasks--test-with-org-buffer
   :input
   "* DONE Title
:PROPERTIES:
:GTASKS-TASKLIST-ID: TASKLIST-ID
:GTASKS-ID: TASK-ID
:GTASKS-ETAG: ETAG
:GTASKS-STATUS: needsAction
:END:
"
   :target
   (with-mock
    (stub org-sync-gtasks--get-or-default-tasklist-id =>
          "TASKLIST-ID")
    (stub org-sync-gtasks--get-gtask-from-cache-or-api =>
          (ht ("title"  "Title")
              ("id"     "TASK-ID")
              ("etag"   "ETAG")
              ("status" "needsAction")))
     (stub org-sync-gtasks--api-tasks-patch =>
           (ht ("title" "Title")
               ("id" "TASK-ID")
               ("etag" "ETAG")
               ("status" "completed")))
     (org-sync-gtasks-at-point))
   :output
   "\\* DONE Title
:PROPERTIES:
:GTASKS-TASKLIST-ID: TASKLIST-ID
:GTASKS-ID: TASK-ID
:GTASKS-ETAG: ETAG
:GTASKS-STATUS: completed
:END:
"))

(ert-deftest org-sync-gtasks-agenda-test/new-tasks ()
  "No new tasks in Google Tasks"
  (org-sync-gtasks--test-with-org-buffer
   :input
   "* TASK
"
   :target
   (with-mock
     (stub org-sync-gtasks--default-tasklist-id =>
           "TASKLIST-ID")
     (stub org-sync-gtasks--make-tasklist-cache =>
           (ht ("TASK-ID"
                (ht ("title"  "TITLE")
                    ("id"     "TASK-ID")
                    ("etag"   "ETAG")
                    ("status" "needsAction")))))
     (org-sync-gtasks-agenda))
   :output
   "\\ TASK
\\* TODO TITLE
:PROPERTIES:
:GTASKS-TASKLIST-ID: TASKLIST-ID
:GTASKS-ID: TASK-ID
:GTASKS-ETAG: ETAG
:GTASKS-STATUS: needsAction
:END:
"))

;; (ert-deftest org-sync-gtasks-agenda-test/completed-gtasks ()
;;   "No new tasks in Google Tasks"
;;   (org-sync-gtasks--test-with-org-buffer
;;    :input
;;    "* TASK
;; "
;;    :target
;;    (with-mock
;;      (stub org-sync-gtasks--default-tasklist-id =>
;;            "TASKLIST-ID")
;;      (stub org-sync-gtasks--make-tasklist-cache =>
;;            (ht ("TASK-ID"
;;                 (ht ("title"  "TITLE")
;;                     ("id"     "TASK-ID")
;;                     ("etag"   "ETAG")
;;                     ("status" "completed")))))
;;      (org-sync-gtasks-agenda))
;;    :output
;;    "\\* TASK
;; ")) ; TODO: this matches even if TASK has properties...

;;; org-sync-gtasks-test.el ends here
