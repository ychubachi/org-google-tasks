;;; org-sync-gtasks.el --- Synchronize Org TODO lists and Google Tasks -*- lexical-binding: t -*-
;; Author: ychubachi
;; URL: https://github.com/ychubachi/org-sync-gtasks
;; Version: 0.9.1
;; Package-Requires: ((emacs "27.1") (org "9.5.2") (oauth2 "0.16") (ht "2.4"))
;;; Commentary:
;; This program is free software
;;; Code:
(require 'org)
(require 'ht)
(require 'org-sync-gtasks-api)
(defun org-sync-gtasks--default-tasklist-id ()
  "This gets the defult tasklist ID."
  (ht-get
   (aref (ht-get (org-sync-gtasks--api-tasklists-list) "items") 0)
   "id"))

(defun org-sync-gtasks--get-or-default-tasklist-id ()
  "Get tasklist id. If it doesn't have GTASKLIST-ID, get it from GTasks."
  (let ((gtasklist-id (org-entry-get nil "GTASKS-TASKLIST-ID")))
    (if (not gtasklist-id)
        (setq gtasklist-id (org-sync-gtasks--default-tasklist-id)))
    gtasklist-id))

(defun org-sync-gtasks--update-todo-headline (tasklist-id gtask)
  "Update headline and its properties."
  (if gtask
      (progn
        ;; Update the headline and its properies.
        (org-edit-headline (ht-get gtask "title"))
        (org-entry-put nil "GTASKS-TASKLIST-ID" tasklist-id)
        (org-entry-put nil "GTASKS-ID" (ht-get gtask "id"))
        (org-entry-put nil "GTASKS-ETAG" (ht-get gtask "etag"))
        (if (ht-get gtask "parent")
            (org-entry-put nil "GTASKS-PARENT" (ht-get gtask "parent")))
        (if (ht-get gtask "notes")
            (org-entry-put nil "GTASKS-NOTES" (ht-get gtask "notes")))
        (let ((status (ht-get gtask "status")))
          (when status
            (org-entry-put nil "GTASKS-STATUS" status)
            (cond
             ((and (equal status "needsAction") (eq (org-entry-get nil "TODO") nil))
              (org-todo "TODO"))
             ((equal status "completed")
              (org-todo "DONE"))
             (t nil))))
        (if (ht-get gtask "due")
            (org-entry-put nil "DEADLINE"
                           (substring (ht-get gtask "due") 0 10)))
        (if (ht-get gtask "completed")
            (org-entry-put nil "GTASKS-COMPLETED" (ht-get gtask "completed")))
        (if (ht-get gtask "deleted")
            (org-entry-put nil "GTASKS-DELETED" "true"))
        (if (ht-get gtask "hidden")
            (org-entry-put nil "GTASKS-HIDDEN" "true")) ; Read only parameter
        ;; TODO: links
        )))

(defun org-sync-gtasks--insert-todo-headline (tasklist-id gtask)
  "Make a new todo headline from GTasks' task."
  (org-insert-todo-heading-respect-content)
  (org-edit-headline (ht-get gtask "title"))
  (org-sync-gtasks--update-todo-headline tasklist-id gtask))

(defun org-sync-gtasks--make-tasklist-cache (tasklist-id)
  "Create a hash table for looking up tasklist or task items by the id."
  (let* ((table (ht-create))
         (tasks (org-sync-gtasks--api-tasks-list tasklist-id))
         (task-items (ht-get tasks "items")))
    (dotimes (i (length task-items))
      (let* ((task (aref task-items i))
             (task-id (ht-get task "id")))
	;; Set the task to the table.
        (ht-set! table task-id task)))
    table))

(defun org-sync-gtasks--get-gtask-from-cache-or-api (tasklist-id task-id cache)
  (if (and cache (ht-get cache task-id))
      (ht-get cache task-id)
    (org-sync-gtasks--api-tasks-get tasklist-id task-id)))

(defun org-sync-gtasks--make-task-from-headline ()
  "Make gtask as a hash table from the headline properties."
  (let ((task     (ht-create))
        (title    (org-entry-get nil "ITEM"))
        (deadline (org-entry-get nil "DEADLINE"))
        (id       (org-entry-get nil "GTASKS-ID"))
        (etag     (org-entry-get nil "GTASKS-ETAG"))
        (notes    (org-entry-get nil "GTASKS-NOTES")))
    (ht-set! task "title" title)
    (if (org-entry-is-todo-p)
        (ht-set! task "status" "needsAction")
      (ht-set! task "status" "completed"))
    (if deadline
        (ht-set! task "due" (format-time-string
                             "%Y-%m-%dT%H:%M:00.000Z"
                             (org-time-string-to-time deadline))))
    (if id     (ht-set! task "id" id))
    (if etag   (ht-set! task "etag" etag))
    (if notes  (ht-set! task "notes" notes))
    task))

(defun org-sync-gtasks--headline-modified-p (gtask)
  (not
   ;; Are the headline and the Google Tasks item same?
   (and
    ;; Is the headline item same?
    (equal (org-entry-get nil "ITEM") (ht-get gtask "title"))
    ;; Is the status same?
    (or
     (and (org-entry-is-todo-p)
          (equal (ht-get gtask "status") "needsAction"))
     (and (org-entry-is-done-p)
          (equal (ht-get gtask "status") "completed")))
    ;; Is the DEADLINE same?
    (or
     (if-let ((deadline (org-entry-get nil "DEADLINE"))
              (gtask-deadline (ht-get gtask "due")))
         (equal (org-time-string-to-time deadline)
                (org-time-string-to-time gtask-deadline)))
     (and (eq (org-entry-get nil "DEADLINE") nil)
          (eq (ht-get gtask "due") nil)))
    ;; TODO: Check 'note' is same
    )))

;;; Entry points
;;;###autoload
(defun org-sync-gtasks-at-point (&optional tasklist-id cache)
  "Synchronize GTasks and an Org todo headline at point.

This command Synchronizes the todo headlines at your cursor.
Make the headline as TODO if not, and create a new GTasks task item.

Push or Pull the task to GTask
  Compare etags. If etags are defferent, get the task from GTasks
  Otherwise, patch the task to GTasks

When etags are same, if those contents are same, nothing to update.
Otherwise, push the task to GTasks.
When etags are not same, pull the task from GTasks.

etags    | contents
-------------------
same     | same     -> Nothing to do.
same     | not same -> Patch remote.
not same | ---      -> Get it from remote.
"
  (interactive)
  ;; Check major-mode
  (if (not (eq major-mode 'org-mode))
      (error "Please use this command in org-mode"))
  ;; Check the headline has ITEM
  (if (not (org-entry-get nil "ITEM"))
      (error "Please use this command at a non-empty Org header"))
  ;; Check TASKLIST-ID
  (if (eq tasklist-id nil)
      (setq tasklist-id (org-sync-gtasks--get-or-default-tasklist-id)))
  ;; Update Org headline if needed.
  (let* ((task (org-sync-gtasks--make-task-from-headline))
         (task-id (ht-get task "id")))
    (org-sync-gtasks--update-todo-headline
     tasklist-id
     (cond
      ;; If the headline is DONE and its status is completed, do nothing.
      ((and (org-entry-is-done-p)
            (equal (org-entry-get nil "GTASKS-STATUS") "completed"))
       (message "GTasks: Completed %s" (ht-get task "title"))
       nil)
      ;; If the task doesn't have gtask-id, Insert a new task to GTasks.
      ((not task-id)
       (message "GTasks: Insert %s" (ht-get task "title"))
       (org-sync-gtasks--api-tasks-insert tasklist-id task))
      (t
       ;; If the task has its task-id, update the headline or GTasks if needed.
       (let* ((gtask (org-sync-gtasks--get-gtask-from-cache-or-api
                      tasklist-id task-id cache)))
         (cond
          ;; If the etag is changed, get the task from Google Tasks.
          ((not (equal (ht-get task "etag") (ht-get gtask "etag")))
           (message "GTasks: Get %s" (ht-get gtask "title"))
           gtask)
          ;; If the headline is modified, patch the task in Google Tasks.
          ((org-sync-gtasks--headline-modified-p gtask)
           (message "GTasks: Patch %s" (ht-get task "title"))
           (org-sync-gtasks--api-tasks-patch tasklist-id task-id task))
          ;; Otherwise, do nothing.
          (t
           (message "GTasks: Keep %s" (ht-get task "title"))
           nil))))))))

;;;###autoload
(defun org-sync-gtasks-agenda ()
  "Synchronize GTasks and Org todo headlines.

Synchronize every todo headlines with GTASKS-ID property.
Also, pull other GTasks tasks as new headines.

Deleted GTasks tasks are also needed to update to change stautus."
  (interactive)
  ;; Check major-mode
  (if (not (eq major-mode 'org-mode))
      (error "Please use this command in org-mode"))
  ;; Make a list of GTASKS-ID by looking up all org TODO headlines in agenda.
  (let* ((tasklist-id (org-sync-gtasks--default-tasklist-id))
         (cache       (org-sync-gtasks--make-tasklist-cache tasklist-id)))
    ;; Update todo headlines with valid GTASKS-ID.
    (org-map-entries
     (lambda ()
       (when-let ((gtasks-id (org-entry-get nil "GTASKS-ID")))
         (org-sync-gtasks-at-point tasklist-id cache)
         (ht-remove! cache gtasks-id))) ; Remove this todo.
     "+TODO={.+}"
     'agenda)
    ;; Insert org headlines from rest of the cache.
    (dolist (gtasks-id (ht-keys cache))
      (when (not (equal (ht-get (ht-get cache gtasks-id) "status") "completed"))
        (message "GTasks: New %s" (ht-get (ht-get cache gtasks-id) "title"))
        (org-sync-gtasks--insert-todo-headline
         tasklist-id
         (ht-get cache gtasks-id)))))
  (message "GTasks: Done"))

(provide 'org-sync-gtasks)
;;; org-sync-gtasks.el ends here
