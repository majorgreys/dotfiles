;;; custom-functions.el -*- lexical-binding: t; -*-

;;
;; Custom functions for org-roam and transcript processing
;;

;;
;; Org-babel utility functions
;;

(defun thb/strip-ansi (text)
  "Remove ANSI color codes from text for clean org-babel output."
  (replace-regexp-in-string "\033\\[[0-9;]*[a-zA-Z]" "" text))

;; Attach files and insert links in References section
(defun thb/org-attach-file-to-references (path)
  "Attach file from PATH and insert link in the References section.

Creates or appends to a '* References' section at the end of the file with
a link to the attached file. Works with local files and URLs.

Supports:
- Local file paths: /path/to/file.pdf
- URLs: https://example.com/file.pdf
- Base64 data URIs for images"
  (interactive "fFile to attach: ")

  (unless (eq major-mode 'org-mode)
    (user-error "Not in an org buffer"))

  (require 'org-download)

  (let ((original-point (point)))
    (condition-case-unless-debug e
        (let* ((raw-uri (url-unhex-string path))
               (new-path (expand-file-name (org-download--fullname raw-uri)))
               (_ (if (string-match-p (concat "^" (regexp-opt '("http" "https" "nfs" "ftp" "file")) ":/") path)
                      (url-copy-file raw-uri new-path)
                    (copy-file path new-path)))
               (rel-path (file-relative-name new-path (file-name-directory (buffer-file-name))))
               (file-name (file-name-nondirectory new-path)))

          ;; Navigate to end of file and find/create References section
          (goto-char (point-max))

          ;; Try to find existing "* References" section
          (if (re-search-backward "^\\* References" nil t)
              (end-of-line)
            ;; Create new section if it doesn't exist
            (goto-char (point-max))
            (unless (bolp) (newline))
            (insert "\n* References")
            (end-of-line))

          ;; Append the link
          (newline)
          (insert (format "- [[file:%s][%s]]" rel-path file-name)))

      (error
       (goto-char original-point)
       (user-error "Failed to attach file: %s" (error-message-string e))))))

;; Org-save-transcript function for processing Slack conversations
(defun thb/org-save-transcript (title tags summary content thread-url)
  "Create a new org-roam note from a Slack transcript with structured formatting.
TITLE: The title for the note
TAGS: Space-separated tags string
SUMMARY: Brief summary of the discussion
CONTENT: Raw transcript content
THREAD-URL: Slack thread URL"
  (let* ((timestamp (format-time-string "%Y%m%d%H%M"))
         (filename (format "%s.org" timestamp))
         (filepath (expand-file-name filename org-roam-directory))
         (raw-tags (if (and tags (not (string-empty-p tags)))
                       (split-string tags " ")
                     '("slack")))
         (tag-list (concat ":" (mapconcat 'identity raw-tags ":") ":"))
         (node-id (org-id-new))
         (formatted-content (thb/format-transcript-content content thread-url summary)))

    ;; Create the node using org-roam-node-create
    (let ((node (org-roam-node-create :title title
                                      :file filepath
                                      :id node-id
                                      :tags raw-tags)))

      ;; Create the file with complete formatted content
      (with-temp-file filepath
        (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n#+title: %s\n#+filetags: %s\n#+date: %s\n#+created: %s\n\n%s"
                       node-id title tag-list
                       (format-time-string "%Y-%m-%d")
                       (format-time-string "[%Y-%m-%d %a %H:%M]")
                       formatted-content)))

      ;; Add to org-roam database
      (org-roam-db-sync)

      ;; Return the filename for confirmation
      (message "Created transcript note: %s" filename)
      filename)))

(defun thb/format-transcript-content (content thread-url summary)
  "Format the transcript content with proper org-mode structure and text wrapping."
  (let ((wrapped-content (thb/wrap-transcript-text content)))
    (concat "* Summary\n\n"
            summary "\n\n"
            "* Raw Transcript\n\n"
            "#+begin_quote\n"
            (if (not (string-empty-p thread-url))
                (concat "Link to the thread: " thread-url "\n\n")
              "")
            wrapped-content
            "\n#+end_quote\n")))

(defun thb/wrap-transcript-text (text)
  "Apply intelligent text wrapping to transcript content for better readability."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))

    ;; Fix EEKS response formatting
    (while (re-search-forward "\\[EEKS @ .* UTC\\]\n:searching-docs: \\*Results For\\*: \"\\([^\"]*\\)\"" nil t)
      (let ((query (match-string 1)))
        (replace-match (format "[EEKS @ %s UTC]\n:searching-docs: *Results For*: \"%s\""
                              (save-excursion
                                (goto-char (match-beginning 0))
                                (if (re-search-forward "\\[EEKS @ \\([^\\]]*\\) UTC\\]" (match-end 0) t)
                                    (match-string 1)
                                  "DATE"))
                              (thb/wrap-line query 80)))))

    ;; Wrap long URLs and separate them from surrounding text
    (goto-char (point-min))
    (while (re-search-forward "<\\(https://[^>|]*\\)|\\([^>]*\\)>" nil t)
      (let ((url (match-string 1))
            (text (match-string 2)))
        (replace-match url)))

    ;; Format numbered responses with better line breaks
    (goto-char (point-min))
    (while (re-search-forward "^\\([0-9]+\\)\\. \\([^-]*\\) -> \\(.*\\)$" nil t)
      (let ((num (match-string 1))
            (question (match-string 2))
            (answer (match-string 3)))
        (replace-match (format "%s. %s\n   -> %s"
                              num
                              (thb/wrap-line question 70)
                              (thb/wrap-line answer 70)))))

    ;; Break long lines at sentence boundaries
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line-start (point))
            (line-end (line-end-position)))
        (when (> (- line-end line-start) 100)
          (goto-char (+ line-start 80))
          (when (re-search-forward "[.!?] \\|, \\|; \\| -> " line-end t)
            (backward-char 1)
            (insert "\n")))
        (forward-line 1)))

    (buffer-string)))

(defun thb/wrap-line (text max-length)
  "Wrap a single line of text at word boundaries."
  (if (<= (length text) max-length)
      text
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (not (eobp))
        (forward-char max-length)
        (when (not (eobp))
          (if (re-search-backward " " (- (point) 20) t)
              (progn
                (delete-char 1)
                (insert "\n"))
            (forward-char 10))))
      (buffer-string))))

;;
;; Interactive transcript processing with org-roam tag integration
;;

(defun thb/get-org-roam-filetags ()
  "Get list of existing filetags from org-roam database."
  (require 'org-roam)
  (org-roam-db-sync)
  (let ((tags-query "SELECT DISTINCT tag FROM tags WHERE tag NOT IN ('claude', 'ATTACH')"))
    (mapcar #'car (org-roam-db-query tags-query))))

(defun thb/process-transcript-file (transcript-file)
  "Process a Slack transcript file by analyzing content with Claude for intelligent tagging.
Reads the transcript file and sends it to Claude with existing org-roam filetags for selection."
  (interactive "fTranscript file: ")
  (let* ((file-path (expand-file-name transcript-file))
         (existing-tags (thb/get-org-roam-filetags))
         (tags-list (mapconcat (lambda (tag) (format "- %s" tag)) existing-tags "\n"))
         (claude-prompt (format "Please analyze this Slack transcript and provide a structured response with:

1. **Title**: A concise, descriptive title for this discussion
2. **Tags**: Select 3-5 most relevant tags from this existing vocabulary:
%s

3. **Summary**: A 2-3 sentence summary highlighting key points and participants

The transcript file is: %s

Please respond in this exact format:
TITLE: [your generated title]
TAGS: [space-separated list of selected tags]
SUMMARY: [your generated summary]

Then use the org-save-transcript command to create the org-roam note." tags-list file-path)))

    ;; Check if file exists
    (unless (file-exists-p file-path)
      (user-error "Transcript file does not exist: %s" file-path))

    ;; Start Claude Code with the transcript processing request
    (let ((default-directory "~/Documents/org/"))
      (async-shell-command
       (format "claude '%s'" claude-prompt)
       "*Claude Transcript Processing*"))

    ;; Show helpful message
    (message "Processing transcript with Claude Code (using existing tags): %s"
             (file-name-nondirectory file-path))))

(defun thb/process-current-file-as-transcript ()
  "Process the current file as a transcript with Claude Code.
Useful when you have a transcript file open in Emacs."
  (interactive)
  (if (buffer-file-name)
      (thb/process-transcript-file (buffer-file-name))
    (user-error "Current buffer is not visiting a file")))

(provide 'custom-functions)