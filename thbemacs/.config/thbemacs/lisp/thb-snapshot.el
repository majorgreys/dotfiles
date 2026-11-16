;;; thb-snapshot.el --- Explicit PNG snapshots of GUI frames -*- lexical-binding: t; -*-

;;; Commentary:
;; The macOS NS build has no x-export-frames.  Capture only the requested
;; window with screencapture; never fall back to a desktop capture or upload.

;;; Code:

(require 'thb-open)

(defcustom thb-snapshot-program "/usr/sbin/screencapture"
  "macOS executable used to capture one Emacs window."
  :type 'file
  :group 'thb-open)

(defun thb-snapshot--capture (window-id output)
  "Capture WINDOW-ID into temporary local OUTPUT without blocking events.
Permission errors are reported without retry or alternate capture."
  (let ((default-directory "/"))
    (with-temp-buffer
      (let ((process (make-process
                      :name "thb-snapshot" :buffer (current-buffer)
                      :command (list thb-snapshot-program "-x" "-o" "-t" "png"
                                     "-l" window-id output)
                      :connection-type 'pipe :noquery t :sentinel #'ignore)))
        (unwind-protect
            (progn
              (with-timeout (30 (error "Frame snapshot timed out; no capture was saved"))
                (while (process-live-p process)
                  (accept-process-output process 0.05)))
              (unless (and (eq (process-status process) 'exit)
                           (zerop (process-exit-status process)))
                (error "Frame snapshot failed; Screen Recording permission may be required: %s"
                       (string-trim (buffer-string))))
              (buffer-string))
          (when (process-live-p process) (delete-process process)))))))

(defun thb-snapshot--png-p (file)
  "Whether FILE is nonempty and starts with a PNG signature."
  (let ((attributes (file-attributes file)))
    (and attributes
         (null (file-attribute-type attributes))
         (> (file-attribute-size attributes) 8)
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (insert-file-contents-literally file nil 0 8)
           (equal (buffer-string) (unibyte-string #x89 ?P ?N ?G 13 10 26 10))))))

;;;###autoload
(defun thb-snapshot-frame (file &optional frame)
  "Save the current GUI FRAME as a local PNG FILE and return its path.
Interactively, prompt for FILE and confirm replacement of an existing file.
Programmatic calls never overwrite.  Capture only this normal Emacs window,
without sound or shadow; do not focus it, capture other windows, or upload.
The original destination is preserved if capture or validation fails."
  (interactive
   (let ((frame (selected-frame))
         (default-directory (file-name-as-directory (getenv "HOME"))))
     (unless (thb-open--normal-frame-p frame)
       (user-error "Select a normal GUI Emacs frame to snapshot"))
     (list (read-file-name "Save frame PNG: " default-directory nil nil
                           (format-time-string "emacs-frame-%Y%m%d-%H%M%S.png"))
           frame)))
  (let* ((frame (or frame (selected-frame)))
         (default-directory "/")
         (output (expand-file-name file)))
    (unless (thb-open--normal-frame-p frame)
      (user-error "Select a normal GUI Emacs frame to snapshot"))
    (when (file-remote-p output)
      (user-error "Choose a local PNG destination; remote output is not supported"))
    (unless (equal (downcase (or (file-name-extension output) "")) "png")
      (user-error "Choose a destination ending in .png"))
    (when (or (file-symlink-p output) (file-directory-p output))
      (user-error "Snapshot destination must be a regular file, not a directory or symlink"))
    (unless (file-directory-p (file-name-directory output))
      (user-error "Snapshot destination directory does not exist"))
    (let* ((original (file-attributes output 'integer))
           (overwrite
            (when original
              (unless (and (called-interactively-p 'interactive)
                           (yes-or-no-p (format "Replace %s? " output)))
                (user-error "Snapshot destination already exists; no capture taken"))
              t))
           (window-id (thb-open--window-id frame))
           ;; Give screencapture a new PNG path in a private directory, rather
           ;; than a pre-created hidden image file.  Stay on the destination
           ;; filesystem so publishing the completed image remains atomic.
           (staging (make-temp-file
                     (expand-file-name ".thb-snapshot-" (file-name-directory output)) t))
           (temporary (expand-file-name "frame.png" staging)))
      (unwind-protect
          (progn
            (redisplay t)
            (let ((diagnostic (thb-snapshot--capture window-id temporary)))
              (unless (thb-snapshot--png-p temporary)
                (error "Capture did not produce a PNG; no capture was saved%s"
                       (if (and (stringp diagnostic)
                                (not (string-empty-p (string-trim diagnostic))))
                           (concat ": " (string-trim diagnostic))
                         ""))))
            (unless (equal original (file-attributes output 'integer))
              (error "Snapshot destination changed during capture; refusing to replace it"))
            (set-file-modes temporary #o600)
            (rename-file temporary output overwrite)
            (message "Saved Emacs frame snapshot: %s" output)
            output)
        (when (file-directory-p staging) (delete-directory staging t))))))

(provide 'thb-snapshot)
;;; thb-snapshot.el ends here
