;;; -*- lexical-binding: t -*-

;; Google Task API Ex
;; https://developers.google.com/tasks/reference/rest/?apix=true

(setq debug-on-error nil)

(require 'ht)
(require 'el-mock)


(defcustom my/file "~/gtasks.org"
  "An Org file."
  :type '(string)
  :group 'org-gtasks-sync)


;;; Parse org tasklist file
(ert-deftest my/parse-tasklists-test ()
  ;; setup
  (with-mock
    ;; (stub my/do-tasklist => nil)
    (stub my/do-task => nil)
    (let* ((org "* Tasklist
** Tas
")
	   (result))
      (with-current-buffer (find-file-noselect (make-temp-file "org"))
	(org-mode)
	(insert org)
	;; target
	(my/parse-tasklists)
	;; test
	(setq result (substring-no-properties (buffer-string)))
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer)))
      (should (equal result "* Tasklist
:PROPERTIES:
:GTASKS-ID: SOMEGTASKSID
:END:
** Task
")))))


;;; Parse org entries
;;

(defun my/parse-tasklists ()
  "Parse org tasklists.
Each level 1 org headline is treated as gtasks tasklist.
Each level 2 org headline is treated as gtasks task.

-  Map entries
  - if level 1,
    - insert or update the tasklist
    - set current id
  - if level 2,
    - if no gtasks id
      - insert it as a task of current id tasklist
    - else
       - update it as a task of current id tasklist
"
  (org-map-entries ; iterate headlines
   (lambda ()
     (let* ((headline (org-element-at-point))
	    (level (org-element-property :level headline))
	    (current-id))
       (cond ((eq level 1)  ; tasklist
	      (setq current-id (my/do-tasklist)))
	     ((eq level 2) ; task
	      (my/do-task current-id))
	     (t nil) ; nothing to do. or, error?
	     )))
   nil))

;;; Do tasklist
;; (ert-deftest my/do-tasklist-test ()
;;   (let* ((org "* Tasklist\n")
;; 	 (result))
;;     (with-current-buffer (find-file-noselect (make-temp-file "org"))
;;       (org-mode)
;;       (insert org)
;;       ;; target
;;       (my/do-tasklist)
;;       ;; test
;;       (setq result (substring-no-properties (buffer-string)))
;;       (set-buffer-modified-p nil)
;;       (kill-buffer (current-buffer)))
;;     (should (equal result "* Tasklist
;; :PROPERTIES:
;; :GTASKS-ID: SOMEGTASKSID
;; :END:
;; "))))

(ert-deftest my/gtasks-update-tasklists-test ()
  (let* ((org "* Tasklist
:PROPERTIES:
:GTASKS-ID: SOMEGTASKSID
:GTASKS-ETAG: \"SOMEGTASKETAG\"
:END:
")
	 (result))
    (with-current-buffer (find-file-noselect (make-temp-file "org"))
      (org-mode)
      (insert org)
      ;; target
      ;; get title, gtasks-id, gtasks-etag, etc from org headline
      (let ((title (org-entry-get nil "ITEM"))
	    (gtasks-id (org-entry-get nil "GTASKS-ID")))
	;; if gtasks-id is nil, call insert method.
	;;   It returns id and etag. set them to headline
	;; if gtasks-id is non-nil, call update (or patch?) method

	(org-entry-put nil "GTASKS-ID" gtasks-id)
	;; return gtasks-id
	)
      ;; test
      ;;(setq result (substring-no-properties (buffer-string)))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    (should (equal result "* Tasklist
:PROPERTIES:
:GTASKS-ID: SOMEGTASKSID
:END:
"))))


;; (defun my/tasklists-insert (token)
;;   (oauth2-url-retrieve-synchronously
;;    token
;;    "https://tasks.googleapis.com/tasks/v1/users/@me/lists"
;;    "POST"
;;    "{\"title\": \"New Tasklist 2\"}"))

(defun my/tasklists-update (token)
  (oauth2-url-retrieve-synchronously
   token
   "https://tasks.googleapis.com/tasks/v1/users/@me/lists"
   "POST"
   "{\"title\": \"New Tasklist 2\"}"))
