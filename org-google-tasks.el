;;; -*- lexical-binding: t -*-

(setq debug-on-error nil)

(require 'ht)

;; tasklists are treated as level 1 headlines.
(defun my/tasklists (file)
  (let ((json-object-type 'hash-table)
	(json-array-type 'list)
	(json-key-type 'symbol))
    (let ((tasklists
	   (json-read-file file)))
      tasklists))) ;; returns tasklists hash table.

(ert-deftest my/tasklists-test ()
  (let ((tasklists (my/tasklists "./test/sample-tasklists.json")))
    (should (equal (hash-table-p tasklists) t))))

;; Insert a headline from gtask/gtasklist item.
;; It will be used to pull the item from Google Tasks.
(defun my/update-or-create-headline (gtasks-item)
  (let* ((title (ht-get gtasks-item 'title))
	 (kind (ht-get gtasks-item 'kind))
	 (id (ht-get gtasks-item 'id))
	 (etag (ht-get gtasks-item 'etag))
	 (updated (ht-get gtasks-item 'updated)))
    ;; (self-link (ht-get gtasks-item 'selfLink))

    ;; already exists?
    ;; find the headline if exists.
    ;; (unless (my-goto-headline (gtasks-id))
    ;;   (insert ..))
    ;; - create new headline
    (insert (format "* %s\n" title))
    ;; - update a headline
    ;; Update properties.
    ;; if TODO, kind=tasks#task
    (if (equal kind "tasks#task")
	(org-todo 'todo))
    (org-id-get-create) ; file-visiting buffer only

    (org-entry-put nil "GTASKS-ID" id)
    (org-entry-put nil "GTASKS-ETAG" etag)
    (org-entry-put nil "GTASKS-UPDATED" updated))
  ;; (org-entry-put nil "GTASKS-SELF-LINK" self-link)
  )

(ert-deftest my/update-or-create-headline_create-test ()
  (let (result)
    (let* ((file "./test/sample-blank.org")
	   (tasklists (my/tasklists "./test/sample-tasklists.json"))
	   (tasklist (ht-get tasklists 'items)))
      (with-current-buffer (find-file-noselect file)
	(erase-buffer)		 ;; test for create
	(let ((item (car tasklist))) ;; one item
	  (my/update-or-create-headline item))
	;; test
	(setq result (substring-no-properties (buffer-string)))
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer)))
      (should (equal
	       (length result)
	       (length "* Tasks
:PROPERTIES:
:ID:       98a4b293-125c-4178-a509-6936ced29d7e
:GTASKS-ID: MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow
:GTASKS-ETAG: \"MjA3MjIyNTAxMA\"
:GTASKS-UPDATED: 2022-03-12T00:24:59.564Z
:END:
"))))))

;; 1 lv要素（ID付き）
(defun my/headlines-from-tasklists (tasklists)
  (let ((items (ht-get tasklists 'items)))	; list of hashtables
    (mapcar
     (lambda (item) (my/update-or-create-headline item))
     items)))
;;
(ert-deftest my/headlines-from-tasklists_create-test ()
  ;; setup
  (let (result)
    (let* ((file "./test/sample-blank.org")
	   (tasklists (my/tasklists "./test/sample-tasklists.json")))
      (with-current-buffer (find-file-noselect file)
	(erase-buffer)		; create
	(gnus-goto-char (point-min))
	;; target
	(my/headlines-from-tasklists tasklists)
	;; test
	(setq result (substring-no-properties (buffer-string)))
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer))))
    (should (equal
	     (length result)
	     (length "* Tasks
:PROPERTIES:
:ID:       99b55708-e513-4424-82c0-f70060aad11f
:GTASKS-ID: MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow
:GTASKS-ETAG: \"MjA3MjIyNTAxMA\"
:GTASKS-UPDATED: 2022-03-12T00:24:59.564Z
:END:
* テスト
:PROPERTIES:
:ID:       99b55708-e513-4424-82c0-f70060aad11f
:GTASKS-ID: MkhZNnRBVGE4eFk2UW1sdw
:GTASKS-ETAG: \"MjA3NDg5NzE5MQ\"
:GTASKS-UPDATED: 2022-03-12T01:09:31.383Z
:END:
")))))

;; (ert-deftest my/headlines-from-tasklists_update-test ()
;;   ;; setup
;;   (let ((result
;; 	 (let* ((file "./test/sample-tasklists_update.org")
;; 		(tasklists (my/tasklists "./test/sample-tasklists.json")))
;; 	   (with-current-buffer (find-file-noselect file)
;; 	     ;; (erase-buffer)
;; 	     ;; target
;; 	     (my/headlines-from-tasklists tasklists)
;; 	     ;; test
;; 	     (substring-no-properties (buffer-string))))))
;;     (with-output-to-temp-buffer "*yc temp out*"
;;       (princ result))
;;     (should (equal
;; 	     (length result)
;; 	     (length "* Tasks
;; :PROPERTIES:
;; :ID:       99b55708-e513-4424-82c0-f70060aad11f
;; :GTASKS-ID: MDc1MzA1NTQ1OTYxODU5MTEwMTg6MDow
;; :GTASKS-ETAG: \"MjA3MjIyNTAxMA\"
;; :GTASKS-UPDATED: 2022-03-12T00:24:59.564Z
;; :END:
;; * テスト
;; :PROPERTIES:
;; :ID:       99b55708-e513-4424-82c0-f70060aad11f
;; :GTASKS-ID: MkhZNnRBVGE4eFk2UW1sdw
;; :GTASKS-ETAG: \"MjA3NDg5NzE5MQ\"
;; :GTASKS-UPDATED: 2022-03-12T01:09:31.383Z
;; :END:
;; ")))))


(defun my/tasks ()
  (let ((json-object-type 'hash-table)
	(json-array-type 'list)
	(json-key-type 'symbol))
    (let ((tasks
	   (json-read-file "./test/sample-tasks.json")))
      tasks)))

(ert-deftest my/tasks-test ()
  (let ((tasks (my/tasks)))
    (should (equal (hash-table-p tasks) t))))

;; itemsを順番に
;;   parent属性がないなら、作成
;;   parent属性があるなら、parent要素を探す
;;     parent要素が見つかったら、parent要素の下位に作成
;;     parent要素が見つからなかったら、itemsの最後に追加する
(defun my/headlines-from-tasks (tasks)
  (let ((task-items (ht-get tasks 'items)))
    (while task-items
      (let ((task (car task-items)))
	;; task has parent and no parent task?
	;; (if (ht-get task 'parent)
	;; 	  )
	(insert (format "** TODO %s\n"      (ht-get task 'title)))
	(org-id-get-create)
	(org-entry-put nil "GTASKS-ID"      (ht-get task 'id))
	(org-entry-put nil "GTASKS-ETAG"    (ht-get task 'etag))
	(org-entry-put nil "GTASKS-UPDATED" (ht-get task 'updated))
	;; next task item
	(setq task-items (cdr task-items))))))

(ert-deftest my/headlines-from-tasks_create-test ()
  ;; setup
  (let (result)
    (let* ((file "./test/sample-blank.org")
	   (tasks (my/tasks)))
      (with-current-buffer (find-file-noselect file)
	;; target
	(my/headlines-from-tasks tasks)
	;; test
	(setq result (substring-no-properties (buffer-string)))
	;; (kill-buffer (current-buffer)) ??? Why we don't need it?
	))
    ;; (with-output-to-temp-buffer "*yc temp out*"
    ;;   (princ result))
    (should (equal nil nil)))) ; todo

;; TODO GTASKS-IDでヘッドラインを検索する
;; (org-map-entries tasks
;; スキャンし，idとposのリストを作る
;; FIXME: Can we obtain only headlines having GTASKS-ID?
;; May not use this.
(defun my/gtasks-id-alist ()
  (org-map-entries
   (lambda ()
     (let ((gtasks-id (org-entry-get nil "GTASKS-ID")))
       (cons gtasks-id (point))))
   "+TODO=\"TODO\""
   ;;"+TODO"
   ;; "+GTASKS-ID=\"cF9ORW8yYWgyNTVES1dIbg\""
   ))

(ert-deftest my/gtasks-id-alist-test ()
  (let (result)
    (let* ((file "./test/sample-tasks.org"))
      (with-current-buffer (find-file-noselect file)
	;; target
	(setq result (my/gtasks-id-alist))
	;; test
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer))))
    (should (equal (length result) 8))))

(defun my/find (gtasks-id)
  (let ((gtasks-id-alist
	 (org-map-entries
	  (lambda ()
	    (let ((gtasks-id (org-entry-get nil "GTASKS-ID")))
	      (cons gtasks-id (point))))
	  "+TODO=\"TODO\"")))
    (cdr (assoc gtasks-id gtasks-id-alist)))) ; returns the position.

(ert-deftest my/find-test ()
  (let (result)
    (let* ((file "./test/sample-tasks.org"))
      (with-current-buffer (find-file-noselect file)
	;; target
	(let ((gtasks-id "Y1hxLXB0ZHJZb0x0Z3I0Mw"))
	  (setq result (my/find gtasks-id)))
	;; test
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer))))
    (should (eq result 589))))
