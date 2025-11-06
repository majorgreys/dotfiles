;;; jira-sync.el --- Fast, deterministic Jira sync using jira-cli -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Tahir H. Butt

;; Author: Tahir H. Butt <tahir@tahirbutt.com>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Fast, deterministic Jira sync for local archive/search use case.
;; Uses ankitpokhrel/jira-cli for API calls instead of org-jira.
;;
;; Usage:
;;   M-x jira-sync-all-projects
;;   M-x jira-sync-project (prompts for project)

;;; Code:

(require 'json)

(defgroup jira-sync nil
  "Fast Jira sync using jira-cli."
  :group 'org)

(defcustom jira-sync-working-dir "~/Documents/org/jira"
  "Directory where Jira org files are stored."
  :type 'directory
  :group 'jira-sync)

(defcustom jira-sync-base-url nil
  "Base URL for Jira instance (e.g., https://mycompany.atlassian.net).
Used to generate issue URLs in synced org files."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "URL"))
  :group 'jira-sync)

(defcustom jira-sync-projects nil
  "List of Jira projects to sync."
  :type '(repeat string)
  :group 'jira-sync)

(defcustom jira-sync-created-after "2025-01-01"
  "Only sync issues created after this date (YYYY-MM-DD)."
  :type 'string
  :group 'jira-sync)

(defcustom jira-sync-open-statuses '("TODO" "In Progress" "Blocked" "Paused" "Reviewable" "Icebox")
  "List of Jira statuses considered 'open' for incremental sync."
  :type '(repeat string)
  :group 'jira-sync)

(defcustom jira-sync-cli-path "jira"
  "Path to jira-cli executable."
  :type 'string
  :group 'jira-sync)

(defun jira-sync--run-command (args)
  "Run jira-cli with ARGS and return output as string."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process jira-sync-cli-path nil t nil args)))
      (if (zerop exit-code)
          (buffer-string)
        (error "jira-cli failed: %s" (buffer-string))))))

(defun jira-sync--get-current-user ()
  "Get current Jira user email."
  (string-trim (jira-sync--run-command '("me"))))

(defun jira-sync--build-jql (project &optional open-only)
  "Build JQL query for PROJECT.
If OPEN-ONLY is non-nil, only fetch issues with open statuses.

Includes issues where user:
- Is currently assigned
- Was ever assigned (historical)
- Transitioned the status
- Made a comment"
  (let ((base (format "project = %s AND created >= '%s' AND (assignee = currentUser() OR assignee WAS currentUser() OR status CHANGED BY currentUser() OR comment ~ currentUser())"
                      project jira-sync-created-after)))
    (if open-only
        (let ((status-list (mapconcat (lambda (s) (format "\"%s\"" s))
                                       jira-sync-open-statuses
                                       ", ")))
          (format "%s AND status IN (%s)" base status-list))
      base)))

(defun jira-sync--fetch-issue-keys (project &optional open-only)
  "Fetch issue keys for PROJECT using JQL.
If OPEN-ONLY is non-nil, only fetch issues with open statuses."
  (let* ((jql (jira-sync--build-jql project open-only))
         (output (jira-sync--run-command
                  (list "issue" "list"
                        "--jql" jql
                        "--paginate" "100"
                        "--plain"
                        "--no-truncate")))
         (lines (split-string output "\n" t))
         (keys '()))
    ;; Skip header line and extract second column (issue key)
    (dolist (line (cdr lines))
      ;; Format: TYPE  KEY  SUMMARY...
      (when (string-match "^[^\t]+\t+\\([A-Z]+-[0-9]+\\)" line)
        (push (match-string 1 line) keys)))
    (nreverse keys)))

(defun jira-sync--extract-text-from-adf (adf-node &optional list-depth in-list-item)
  "Extract and format text from Jira's Atlassian Document Format (ADF) NODE.
LIST-DEPTH tracks nesting level for proper list indentation.
IN-LIST-ITEM is t when processing content inside a list item."
  (when adf-node
    (let ((node-type (cdr (assoc 'type adf-node)))
          (content (cdr (assoc 'content adf-node)))
          (text (cdr (assoc 'text adf-node)))
          (marks (cdr (assoc 'marks adf-node)))
          (attrs (cdr (assoc 'attrs adf-node))))
      (cond
       ;; Text node with optional inline formatting
       (text
        (let ((formatted-text text))
          ;; Apply inline marks (bold, italic, code, links)
          (when marks
            (dolist (mark (append marks nil))
              (let ((mark-type (cdr (assoc 'type mark))))
                (cond
                 ((equal mark-type "strong")
                  (setq formatted-text (format "*%s*" formatted-text)))
                 ((equal mark-type "em")
                  (setq formatted-text (format "/%s/" formatted-text)))
                 ((equal mark-type "code")
                  (setq formatted-text (format "~%s~" formatted-text)))
                 ((equal mark-type "link")
                  (let ((href (cdr (assoc 'href (cdr (assoc 'attrs mark))))))
                    (when href
                      (setq formatted-text (format "[[%s][%s]]" href formatted-text)))))))))
          formatted-text))

       ;; Node with content array
       (content
        (let* ((children (mapcar (lambda (child)
                                   (jira-sync--extract-text-from-adf child list-depth in-list-item))
                                 (append content nil)))
               (result
                (cond
                 ;; Paragraph - no double newline if inside list item
                 ((equal node-type "paragraph")
                  (if in-list-item
                      (mapconcat #'identity (remove "" children) "")
                    (concat (mapconcat #'identity (remove "" children) "") "\n\n")))

                 ;; Headings
                 ((equal node-type "heading")
                  (let ((level (or (cdr (assoc 'level attrs)) 1)))
                    (concat (make-string (+ level 2) ?*) " "
                            (mapconcat #'identity (remove "" children) "")
                            "\n\n")))

                 ;; Code block
                 ((equal node-type "codeBlock")
                  (let ((language (or (cdr (assoc 'language attrs)) "")))
                    (concat "#+begin_src " language "\n"
                            (mapconcat #'identity (remove "" children) "")
                            "\n#+end_src\n\n")))

                 ;; Block quote
                 ((equal node-type "blockquote")
                  (concat "#+begin_quote\n"
                          (mapconcat #'identity (remove "" children) "")
                          "#+end_quote\n\n"))

                 ;; Bullet list
                 ((equal node-type "bulletList")
                  (concat (mapconcat #'identity (remove "" children) "")
                          (if (zerop (or list-depth 0)) "\n" "")))

                 ;; Ordered list
                 ((equal node-type "orderedList")
                  (let ((items (remove "" children))
                        (counter 1))
                    (concat (mapconcat (lambda (item)
                                         (prog1
                                             (replace-regexp-in-string
                                              "^- " (format "%d. " counter) item)
                                           (setq counter (1+ counter))))
                                       items "")
                            (if (zerop (or list-depth 0)) "\n" ""))))

                 ;; List item - process children with in-list-item flag
                 ((equal node-type "listItem")
                  (let* ((indent (make-string (* 2 (or list-depth 0)) ?\s))
                         ;; Re-process children with in-list-item=t
                         (list-children (mapcar (lambda (child)
                                                  (jira-sync--extract-text-from-adf child list-depth t))
                                                (append content nil)))
                         (item-content (mapconcat #'identity (remove "" list-children) ""))
                         ;; Strip trailing newlines from content to avoid extra blank lines
                         (trimmed-content (replace-regexp-in-string "\n+$" "" item-content)))
                    (concat indent "- " trimmed-content "\n")))

                 ;; Hard break
                 ((equal node-type "hardBreak")
                  "\n")

                 ;; Inline/block cards (links, mentions, etc.)
                 ((or (equal node-type "inlineCard")
                      (equal node-type "blockCard"))
                  (let ((url (cdr (assoc 'url attrs))))
                    (if url
                        (format "[[%s]]" url)
                      "")))

                 ;; Mention
                 ((equal node-type "mention")
                  (let ((mention-text (cdr (assoc 'text attrs))))
                    (or mention-text "")))

                 ;; Media (images, attachments)
                 ((equal node-type "media")
                  ;; Just skip media nodes as we can't embed them
                  "")

                 ;; Unknown node types - just concat children
                 (t
                  (mapconcat #'identity (remove "" children) "")))))
          result))

       ;; Empty node
       (t "")))))

(defun jira-sync--format-comments (comments-array)
  "Format COMMENTS-ARRAY from Jira API to org-mode text."
  (when (and comments-array (> (length comments-array) 0))
    (mapconcat
     (lambda (comment)
       (let* ((author (cdr (assoc 'displayName (cdr (assoc 'author comment)))))
              (created (cdr (assoc 'created comment)))
              (body (cdr (assoc 'body comment))))
         (format "%s - %s\n%s"
                 (or author "Unknown")
                 (substring created 0 10)  ; Extract date only
                 (jira-sync--extract-text-from-adf body))))
     comments-array
     "\n\n")))

(defun jira-sync--fetch-issue-details (key)
  "Fetch full details for issue KEY using raw JSON API response."
  (let* ((output (jira-sync--run-command (list "issue" "view" key "--raw")))
         (json-object (json-read-from-string output))
         (fields (cdr (assoc 'fields json-object)))
         (issue (make-hash-table :test 'equal)))

    ;; Basic fields
    (puthash 'key key issue)
    (puthash 'summary (or (cdr (assoc 'summary fields)) "") issue)
    (puthash 'type (or (cdr (assoc 'name (cdr (assoc 'issuetype fields)))) "Task") issue)
    (puthash 'status (or (cdr (assoc 'name (cdr (assoc 'status fields)))) "") issue)

    ;; Priority
    (let ((priority (cdr (assoc 'priority fields))))
      (when priority
        (puthash 'priority (cdr (assoc 'name priority)) issue)))

    ;; Assignee
    (let ((assignee (cdr (assoc 'assignee fields))))
      (when assignee
        (puthash 'assignee (cdr (assoc 'displayName assignee)) issue)))

    ;; Reporter
    (let ((reporter (cdr (assoc 'reporter fields))))
      (when reporter
        (puthash 'reporter (cdr (assoc 'displayName reporter)) issue)))

    ;; Dates (extract just the date part)
    (let ((created (cdr (assoc 'created fields))))
      (when created
        (puthash 'created (substring created 0 10) issue)))

    (let ((updated (cdr (assoc 'updated fields))))
      (when updated
        (puthash 'updated (substring updated 0 10) issue)))

    ;; Parent (for subtasks)
    (let ((parent (cdr (assoc 'parent fields))))
      (when parent
        (puthash 'parent (cdr (assoc 'key parent)) issue)))

    ;; Labels
    (let ((labels (cdr (assoc 'labels fields))))
      (when (and labels (> (length labels) 0))
        (puthash 'labels (mapconcat 'identity labels ", ") issue)))

    ;; Description
    (let ((desc (cdr (assoc 'description fields))))
      (when desc
        (puthash 'description (jira-sync--extract-text-from-adf desc) issue)))

    ;; Comments
    (let* ((comment-obj (cdr (assoc 'comment fields)))
           (comments (cdr (assoc 'comments comment-obj))))
      (when comments
        (puthash 'comments (jira-sync--format-comments comments) issue)))

    issue))

(defun jira-sync--map-status-to-todo (status)
  "Map Jira STATUS to org TODO keyword."
  (let ((status-lower (downcase (or status ""))))
    (cond
     ((string-match-p "done\\|closed\\|resolved" status-lower) "DONE")
     ((string-match-p "cancelled\\|won't" status-lower) "CANCELLED")
     ((string-match-p "in review" status-lower) "IN-REVIEW")
     ((string-match-p "in progress" status-lower) "IN-PROGRESS")
     ((string-match-p "blocked" status-lower) "BLOCKED")
     ((string-match-p "paused\\|on hold" status-lower) "PAUSED")
     ((string-match-p "reviewable" status-lower) "REVIEWABLE")
     ((string-match-p "icebox" status-lower) "ICEBOX")
     ((string-match-p "backlog" status-lower) "BACKLOG")
     (t "TODO"))))

(defun jira-sync--normalize-whitespace (text)
  "Normalize whitespace in TEXT for clean org-mode formatting.
- Collapse multiple consecutive newlines to max 2
- Trim leading/trailing whitespace
- Remove blank lines at start/end"
  (when text
    (setq text (string-trim text))
    ;; Collapse 3+ consecutive newlines to 2
    (setq text (replace-regexp-in-string "\n\n\n+" "\n\n" text))
    ;; Remove spaces at end of lines
    (setq text (replace-regexp-in-string " +$" "" text))
    text))

(defun jira-sync--escape-org-text (text)
  "Normalize and clean TEXT for org-mode format."
  (when text
    (jira-sync--normalize-whitespace text)))

(defun jira-sync--issue-to-org (issue)
  "Convert ISSUE hash table to org-mode string with property drawer."
  (let* ((key (gethash 'key issue))
         (summary (or (gethash 'summary issue) "No summary"))
         (tag (format ":%s:" (replace-regexp-in-string "-" "_" key)))
         (description (gethash 'description issue))
         (comments (gethash 'comments issue)))

    (with-temp-buffer
      ;; Heading with just summary and key tag
      (insert (format "** %s %s\n" summary tag))

      ;; Properties drawer with all metadata
      (insert ":PROPERTIES:\n")
      (insert (format ":ID:         %s\n" key))
      (insert (format ":KEY:        %s\n" key))
      (when jira-sync-base-url
        (insert (format ":URL:        %s/browse/%s\n" jira-sync-base-url key)))

      ;; Core fields
      (when-let ((type (gethash 'type issue)))
        (insert (format ":TYPE:       %s\n" type)))
      (when-let ((status (gethash 'status issue)))
        (insert (format ":STATUS:     %s\n" status)))
      (when-let ((priority (gethash 'priority issue)))
        (insert (format ":PRIORITY:   %s\n" priority)))

      ;; People
      (when-let ((assignee (gethash 'assignee issue)))
        (insert (format ":ASSIGNEE:   %s\n" assignee)))
      (when-let ((reporter (gethash 'reporter issue)))
        (insert (format ":REPORTER:   %s\n" reporter)))

      ;; Dates
      (when-let ((created (gethash 'created issue)))
        (insert (format ":CREATED:    %s\n" created)))
      (when-let ((updated (gethash 'updated issue)))
        (insert (format ":UPDATED:    %s\n" updated)))

      ;; Relationships
      (when-let ((parent (gethash 'parent issue)))
        (insert (format ":PARENT:     %s\n" parent)))
      (when-let ((labels (gethash 'labels issue)))
        (insert (format ":LABELS:     %s\n" labels)))

      (insert ":END:\n")

      ;; Description
      (when (and description (not (string-empty-p (string-trim description))))
        (insert "*** Description\n")
        (insert (jira-sync--escape-org-text description))
        (insert "\n"))

      ;; Comments
      (when (and comments (not (string-empty-p (string-trim comments))))
        (insert "*** Comments\n")
        (let ((comment-blocks (split-string comments "\n\n+" t))
              (comment-num 1))
          (dolist (block comment-blocks)
            (when (not (string-empty-p (string-trim block)))
              (insert (format "**** Comment %d\n" comment-num))
              (insert ":PROPERTIES:\n")
              (insert (format ":ID:       %s_comment_%d\n" key comment-num))
              (insert ":END:\n")
              (insert (jira-sync--escape-org-text block))
              (insert "\n")
              (setq comment-num (1+ comment-num))))))

      (buffer-string))))

(defun jira-sync--sync-project (project &optional open-only)
  "Sync issues for PROJECT to org file.
If OPEN-ONLY is non-nil, only sync issues with open statuses."
  (message "Fetching %s issues%s..." project (if open-only " (open only)" ""))
  (let* ((keys (jira-sync--fetch-issue-keys project open-only))
         (total (length keys))
         (output-file (expand-file-name (format "%s.org" project) jira-sync-working-dir))
         (counter 0))

    (if (null keys)
        (message "No issues found for %s" project)

      (with-temp-file output-file
        ;; File header - clean, no TODO keywords needed
        (insert (format "#+TITLE: %s Jira Issues\n" project))
        (insert (format "#+FILETAGS: :jira:%s:\n\n" (downcase project)))
        (insert (format "* %s\n" project))

        (dolist (key keys)
          (setq counter (1+ counter))
          (message "Fetching %s (%d/%d)..." key counter total)
          (condition-case err
              (let ((issue (jira-sync--fetch-issue-details key)))
                (insert (jira-sync--issue-to-org issue)))
            (error
             (message "Error fetching %s: %s" key (error-message-string err))))))

      (message "Synced %d issues to %s" total output-file))))

;;;###autoload
(defun jira-sync-project (project)
  "Sync all issues for PROJECT asynchronously (including completed)."
  (interactive
   (list (completing-read "Project: " jira-sync-projects nil t)))
  (require 'async)
  (message "Starting async sync for %s..." project)
  (async-start
   `(lambda ()
      (load-file ,(expand-file-name "jira-sync.el" doom-user-dir))
      (jira-sync--sync-project ,project nil)
      ,project)
   (lambda (proj)
     (message "Async sync completed for %s" proj))))

;;;###autoload
(defun jira-sync-project-incremental (project)
  "Sync only open/active issues for PROJECT asynchronously (faster).
Only fetches issues with statuses: TODO, In Progress, Blocked, etc."
  (interactive
   (list (completing-read "Project: " jira-sync-projects nil t)))
  (require 'async)
  (message "Starting async incremental sync for %s..." project)
  (async-start
   `(lambda ()
      (load-file ,(expand-file-name "jira-sync.el" doom-user-dir))
      (jira-sync--sync-project ,project t)
      ,project)
   (lambda (proj)
     (message "Async incremental sync completed for %s" proj))))

;;;###autoload
(defun jira-sync-all-projects ()
  "Sync all projects asynchronously without blocking Emacs UI (including completed issues).
Uses separate Emacs process to perform sync in background."
  (interactive)
  (require 'async)
  (message "Starting async Jira sync...")
  (async-start
   ;; What to do in background process
   `(lambda ()
      (load-file ,(expand-file-name "jira-sync.el" doom-user-dir))
      (let ((start-time (current-time)))
        (dolist (project ',jira-sync-projects)
          (jira-sync--sync-project project nil))
        (float-time (time-subtract (current-time) start-time))))
   ;; What to do when complete
   (lambda (elapsed)
     (message "Async Jira sync completed in %.2f seconds" elapsed))))

;;;###autoload
(defun jira-sync-all-projects-incremental ()
  "Sync only open/active issues asynchronously without blocking Emacs UI.
Only fetches issues with statuses: TODO, In Progress, Blocked, etc.
Uses separate Emacs process to perform sync in background."
  (interactive)
  (require 'async)
  (message "Starting async incremental Jira sync...")
  (async-start
   ;; What to do in background process
   `(lambda ()
      (load-file ,(expand-file-name "jira-sync.el" doom-user-dir))
      (let ((start-time (current-time)))
        (dolist (project ',jira-sync-projects)
          (jira-sync--sync-project project t))
        (float-time (time-subtract (current-time) start-time))))
   ;; What to do when complete
   (lambda (elapsed)
     (message "Async incremental Jira sync completed in %.2f seconds" elapsed))))

(provide 'jira-sync)
;;; jira-sync.el ends here
