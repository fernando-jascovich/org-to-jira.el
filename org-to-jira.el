;;; org-to-jira.el --- Sync Jira worklogs from org clocks.

;; Author: Fernando Jascovich
;; Keywords: jira, org, clock, worklog, convenience
;; Version: 0.1
;; Url: https://github.com/fernando-jascovich/org-to-jira.el
;; Package-Requires: ((emacs "24.3"), (request "0.3"))

;; This file is NOT part of GNU Emacs
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org to jira sync given some assumptions.
;; Please check readme.md for more details.

;;; Code:
;;; -*- lexical-binding: t; -*-
(require 'parse-time)
(require 'request)
(setq-default jira-host ""
              jira-user ""
              jira-pass "")

(defun org-get-entry-level ()
  "Return org entry level."
  (nth 0 (org-heading-components)))

(defun org-get-entry-name ()
  "Return org entry name."
  (nth 4 (org-heading-components)))

(defun org-get-issue-name ()
  "Return org entry name matching expected nested level."
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
  "Detects if STR is a valid Jira key."
  (let (matches)
    (setq matches (string-match "[A-Z0-9]+\-[0-9]+" str))
    (not (eq matches 0))))

(defun org-get-clocks (key &optional date)
  "Extract clocks from org-entry matching KEY.
Optionally, clocks will be filtered using DATE."
  (let (name (found nil) (clocks '()))
    (while (outline-previous-heading))
    (setq name (org-get-entry-name))
    (if (not (string-equal key name))
        (while (and (not found) (outline-next-heading))
          (setq name (org-get-entry-name))
          (if (string-equal name key)
              (setq found t))))

    (while (and (eq (forward-line) 0) (not (outline-on-heading-p)))
      (let ((line (substring-no-properties (thing-at-point 'line))) clock)
        (if (string-match-p "CLOCK:" line)
            (progn
              (setq clock (split-string line "CLOCK: " t split-string-default-separators))
              (if date
                  (if (string-match-p (regexp-quote date) (car clock))
                      (add-to-list 'clocks (car clock) t))
                (add-to-list 'clocks (car clock) t))
              ))
        ))
    clocks))

(defun get-jira-worked-time (clock)
  "Convert org CLOCK string to jira compatible string."
  (let (splitted)
    (setq splitted (split-string clock ":"))
    (format "%sh %sm" (nth 0 splitted) (nth 1 splitted))))

(defun get-jira-start-date (datestring)
  "Get start date and time from DATESTRING for Jira."
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
  "Jira REST auth string."
  (concat "Basic "
          (base64-encode-string (concat jira-user ":" jira-pass))))

(defun do-add-worklog (key start worked)
  "Effectively add worklog to Jira.
Using KEY as ticket, START as start data and WORKED as duration time."
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
  "Check for duplicate worklog on Jira.
For ticket KEY, at date STARTED and WORKED duration."
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
  "Entry point for adding worklog to Jira ticket KEY with org CLOCK."
  (let (dates worked start)
    (setq clock (split-string clock "=>" t "\s*"))
    (setq dates (split-string (car clock) "--"))
    (setq worked (get-jira-worked-time (nth 1 clock)))
    (setq start (get-jira-start-date (car dates)))
    (check-for-existing-worklog key start worked)))

(defun issues-from-current-file ()
  "Get all keys from current org file."
  (save-excursion
    (goto-char (point-max))
    (let ((issues '()))
      (while (outline-previous-heading))
      (let (key p matches)
        (while (outline-next-heading)
          (setq p (point))
          (setq key (org-get-issue-name))
          (setq matches (string-match "[A-Z0-9]+\-[0-9]+" key))
          (if (eq matches 0)
              (add-to-list 'issues key))
          (goto-char p)))
      issues)))

(defun org-to-jira ()
  "Syncs current org buffer to jira."
  (interactive)
  (let ((issues (issues-from-current-file)))
    (mapcar (lambda (issue)
              (let (clocks)
                (setq clocks (org-get-clocks issue))
                (mapcar (lambda (el) (add-worklog issue el)) clocks)))
            issues)))

(defun org-to-jira-entry ()
  "Syncs current org entry to jira."
  (interactive)
  (save-excursion
    (let ((key (org-get-issue-name)))
      (if (not (is-jira-key key))
          (progn
            (let (clocks)
              (setq clocks (org-get-clocks key))
              (mapcar (lambda (el) (add-worklog key el)) clocks)))
        (message "Not in an issue, exitting")))))

(defun org-to-jira-today ()
  "Syncs entries on current org file from today only."
  (interactive)
  (org-to-jira-date (format-time-string "%Y-%m-%d")))

(defun org-to-jira-date (date)
  "Sync entries on current org file using DATE as filter.
DATE should be in ISO format: %Y-%m-%d, i.e.: 2020-01-23."
  (interactive "sDate: ")
  (save-excursion
    (let ((issues (issues-from-current-file))
          (clocks nil))
      (mapcar
       (lambda (issue)
         (setq clocks (org-get-clocks issue date))
         (if clocks
             (mapcar (lambda (el) (add-worklog issue el)) clocks)))
       issues))))

(provide 'org-to-jira)
;;; org-to-jira.el ends here
