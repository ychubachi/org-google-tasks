;;; org-sync-gtasks.el --- Syncronize Org TODO lists with Google Tasks -*- lexical-binding: t -*-
;; Author: ychubachi
;; URL: https://github.com/ychubachi/org-sync-gtasks
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (org "9.5.2") (oauth2 "0.16") (ht "2.4"))
;;; Commentary:
;; This program is free software
;;; Code:

(require 'org)
(require 'ht)
(require 'org-sync-gtasks-api)

;;; Org headlines related functions.
(defun org-sync-gtasks--default-tasklist-id ()
  "This gets the defult tasklist ID."
  (ht-get
   (aref (ht-get (org-sync-gtasks--api-tasklists-list) "items") 0)
   "id"))

(defun org-sync-gtasks--make-tasklist-cache (tasklist-id)
  "Create a hash table for looking up tasklist or task items by the id."
  ;; TODO: Do only defualt tasklist. Give tasklist id.
  (let* ((table (ht-create))
         (tasks (org-sync-gtasks--api-tasks-list tasklist-id))
         (task-items (ht-get tasks "items")))
    (dotimes (i (length task-items))
      (let* ((task (aref task-items i))
            (task-id (ht-get task "id")))
	;; Set the task to the table.
        (ht-set! table task-id task)))
    table))

(defun org-sync-gtasks--get-or-default-tasklist-id ()
  "Get tasklist id. If it doesn't have GTASKLIST-ID, get it from GTasks."
  (let ((gtasklist-id (org-entry-get nil "GTASKS-TASKLIST-ID"))) ; GTASKS-TASKLIST-ID
    (if (eq gtasklist-id nil)
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
            (org-entry-put nil "DEADLINE" (ht-get gtask "due")))
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

(defun org-sync-gtasks--make-task-from-headline ()
  "Make gtask as a hash table from the headline properties."
  (let ((gtask    (ht-create))
        (title    (org-entry-get nil "ITEM"))
        (id       (org-entry-get nil "GTASKS-ID"))
        (etag     (org-entry-get nil "GTASKS-ETAG"))
        (todo     (org-entry-get nil "TODO"))
        (deadline (org-entry-get nil "DEADLINE"))
        (notes    (org-entry-get nil "GTASKS-NOTES")))
    (ht-set! gtask "title" title)
    (if id   (ht-set! gtask "id"    id))
    (if etag (ht-set! gtask "etag"  etag))
    (if (equal todo "DONE")
        (ht-set! gtask "status" "completed")
      (ht-set! gtask "status" "needsAction"))
    (if deadline
        (ht-set! gtask "due" (format-time-string "%Y-%m-%dT%H:%M:00.000Z"
                                                 (date-to-time deadline))))
    (if notes
        (ht-set! gtask "notes" notes))
    gtask))

;;; Utilities
(defun org-sync-gtasks--get-gtask-from-cache-or-api (tasklist-id task-id cache)
  (if (and cache (ht-get cache task-id))
      (ht-get cache task-id)
    (org-sync-gtasks--api-tasks-get tasklist-id task-id)))

(defun org-sync-gtasks--equal-p (task gtask)
  (and
   (equal (ht-get task "title") (ht-get gtask "title"))
   (equal (ht-get task "status") (ht-get gtask "status")))
  ;; TODO: Other properties.
  )

;;; Entry points
;;;###autoload
(defun org-sync-gtasks-sync-at-point (&optional tasklist-id cache)
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
  (if (eq tasklist-id nil)
      (setq tasklist-id (org-sync-gtasks--get-or-default-tasklist-id))) ; API
  (let* ((task (org-sync-gtasks--make-task-from-headline)) ; TODO: gtask -> task
         (task-id (ht-get task "id")))

    ;; Update Org headline's properties.
    (org-sync-gtasks--update-todo-headline
     tasklist-id
     (cond
      ((eq (ht-get task "title") nil)
       nil)
      ((equal (ht-get task "status") "completed")
       (message "GTasks: Completed %s" (ht-get task "title"))
       nil)
      ((eq task-id nil)
           ;; If the task doesn't have gtask-id, Insert a new task to GTasks.
           (message "GTasks: Insert %s" (ht-get task "title"))
           (org-sync-gtasks--api-tasks-insert tasklist-id task))
      (t
       ;; If the task has its task-id, update the headline or GTasks if needed.
       (let ((gtask (org-sync-gtasks--get-gtask-from-cache-or-api
                     tasklist-id task-id cache)))
         (cond
          ((equal (ht-get task "etag") (ht-get gtask "etag"))
           (cond
            ((org-sync-gtasks--equal-p task gtask)
             ;; Nothing to update the headline.
             (message "GTasks: Keep %s" (ht-get task "title"))
             nil)
            (t
             ;; Update the GTasks.
             (message "GTasks: Patch %s" (ht-get task "title"))
             (org-sync-gtasks--api-tasks-patch tasklist-id task-id task))))
          (t
           ;; Update the headline.
           (message "GTasks: Update %s" (ht-get task "title"))
           gtask))))))))

;;;###autoload
(defun org-sync-gtasks-sync-agenda () ; TODO: Test
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
         (table       (org-sync-gtasks--make-tasklist-cache tasklist-id)))
    (org-map-entries
     (lambda ()
       ;; Update todo headlines with valid GTASKS-ID.
       ;; TODO: Use the cache to determin the headline is need to update.
       (let ((gtasks-id (org-entry-get nil "GTASKS-ID")))
         (when (and  gtasks-id
                     ;;(not (equal gtasks-id ""))
                     ;;(ht-get table gtasks-id) ;;
                     )
           (org-sync-gtasks-sync-at-point tasklist-id table)
           (ht-remove! table gtasks-id)))) ; Remove this todo.
     "+TODO={.+}"
     'agenda)
    ;; Insert org headlines from rest of the table.
    (dolist (gtasks-id (ht-keys table))
      (message "GTasks: New %s" (ht-get (ht-get table gtasks-id) "title"))
      (org-sync-gtasks--insert-todo-headline
       tasklist-id
       (ht-get table gtasks-id))))
  (message "GTasks: Done"))

(provide 'org-sync-gtasks)
;;; org-sync-gtasks.el ends here
