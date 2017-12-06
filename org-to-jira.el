
;;; -*- lexical-binding: t; -*-
(require 'parse-time)
(require 'request)
(setq-default jira-host ""
	      jira-user ""
	      jira-pass "")

(defun org-get-entry-level ()
  (nth 0 (org-heading-components)))

(defun org-get-entry-name ()
  (nth 4 (org-heading-components)))

(defun org-get-issue-name ()
  (let (level)
    (setq level (org-get-entry-level))
    (if (> level 2)
	(progn
	  (while (and (> level 2) (outline-previous-heading))
	    (setq level (org-get-entry-level))))
      (progn
	(while (and (< level 2) (outline-next-heading))
	  (setq level (org-get-entry-level)))))
    (org-get-entry-name)))

(defun is-jira-key (str)
  "Detects if STR is a valid Jira key"
  (let (matches)
    (setq matches (string-match "[A-Z]+\-[0-9]+" str))
    (not (eq matches 0))))

(defun org-get-clocks (key)
  (let (name (found nil) (clocks '()))
    (while (outline-previous-heading))
    (setq name (org-get-entry-name))
    (if (not (string-equal key name))
	(while (and (not found) (outline-next-heading))
	  (setq name (org-get-entry-name))
	  (if (string-equal name key)
	      (setq found t))))

    (while (and (eq (forward-line) 0) (not (outline-on-heading-p)))
      (let ((line (thing-at-point 'line)) clock)
	(if (string-match-p "CLOCK:" line)
	    (progn
	      (setq clock (split-string line "CLOCK: " t split-string-default-separators))
	      (add-to-list 'clocks (car clock) t)
	      ))
	))
    clocks))

(defun get-jira-worked-time (clock)
  (let (splitted)
    (setq splitted (split-string clock ":"))
    (format "%sh %sm" (nth 0 splitted) (nth 1 splitted))))

(defun get-jira-start-date (datestring)
  (let (date)
    (setq date (parse-time-string datestring))
    (format "%d-%02d-%02dT%02d:%02d:%02d.000+0000"
	    (nth 5 date)
	    (nth 4 date)
	    (nth 3 date)
	    (nth 2 date)
	    (nth 1 date)
	    (nth 0 date))))

(defun get-jira-auth ()
  (concat "Basic "
	  (base64-encode-string (concat jira-user ":" jira-pass))))

(defun do-add-worklog (key start worked)
  (message "Updating %s..." key)
  (let (data headers)
    (setq data '()
	  headers '())
    (add-to-list 'data (cons "started" start))
    (add-to-list 'data (cons "timeSpent" worked))
    (add-to-list 'data (cons "comment" ""))
    (add-to-list 'headers (cons "Content-Type" "application/json"))
    (add-to-list 'headers (cons "Authorization" (get-jira-auth)))
    (request
     (format "%s/rest/api/2/issue/%s/worklog" jira-host key)
     :type "POST"
     :data (json-encode data)
     :parser 'json-read
     :headers headers
     :success
     (cl-function (lambda (&key data &allow-other-keys)
		    (message "Successfully updated %s" key)))
     :error
     (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
		    (message "ERROR - updating worklog for issue %s, starting %s. %s"
			     key
			     start
			     error-thrown)))
     )))

(defun check-for-existing-worklog (key started worked)
  (message "Checking for duplicate worklog on %s..." key)
  (let (headers)
    (setq headers '())
    (add-to-list 'headers (cons "Content-Type" "application/json"))
    (add-to-list 'headers (cons "Authorization" (get-jira-auth)))
    (request
     (format "%s/rest/api/2/issue/%s/worklog" jira-host key)
     :headers headers
     :sync t
     :success
     (cl-function (lambda (&key data &allow-other-keys)
    		    (let (worklogs exists)
    		      (setq worklogs (assoc 'worklogs data))
		      (setq exists nil)
		      (mapcar (lambda (item)
				(let (d)
				  (setq d (cdr (assoc 'started item)))
				  (if (string-equal d started)
				      (setq exists t))))
			      (cdr worklogs))
		      (if (eq exists t)
		      	  (message "WARN - Worklog already exists: %s, %s" key started)
		      	(do-add-worklog key started worked))
		      )))
     :error
     (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
    		    (message "Got error: %s" error-thrown)))
     :parser 'json-read)))

(defun add-worklog (key clock)
  (let (dates worked start)
    (setq clock (split-string clock "=>" t "\s*"))
    (setq dates (split-string (car clock) "--"))
    (setq worked (get-jira-worked-time (nth 1 clock)))
    (setq start (get-jira-start-date (car dates)))
    (check-for-existing-worklog key start worked)))

(defun org-to-jira ()
  "Syncs current org buffer to jira"
  (interactive)
  (let (issues initial-point)
    (setq initial-point (point))
    (setq issues '())
    (while (outline-previous-heading))
    (let (key p matches)
      (while (outline-next-heading)
	(setq p (point))
	(setq key (org-get-issue-name))
	(setq matches (string-match "[A-Z]+\-[0-9]+" key))
	(if (eq matches 0)
	    (add-to-list 'issues key))
	(goto-char p)))
    (message "%s" (remove-duplicates issues))
    (mapcar (lambda (issue)
	      (let (clocks)
		(setq clocks (org-get-clocks issue))
		(mapcar (lambda (el) (add-worklog issue el)) clocks)))
	    issues)
    (goto-char initial-point)))

(defun org-to-jira-entry ()
  "Syncs current org entry to jira"
  (interactive)
  (let (initial-point key)
    (setq key (org-get-issue-name))
    (if (not (is-jira-key key))
	(progn
	  (let (clocks)
	    (setq clocks (org-get-clocks key))
	    (mapcar (lambda (el) (add-worklog key el)) clocks)))
      (message "Not in an issue, exitting"))
    ))

(provide 'org-to-jira)
;;; org-to-jira.el ends here
