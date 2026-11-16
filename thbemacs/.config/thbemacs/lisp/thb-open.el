;;; thb-open.el --- Open files in a focused GUI frame -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared GUI entry point for explicit Pi and Rex open requests.  Transport and
;; conversation discovery belong to the callers, not to this module.  Existing
;; TRAMP filenames work through ordinary Emacs file handling.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(defgroup thb-open nil
  "Open files and directories in GUI thbemacs."
  :group 'files)

(defcustom thb-open-workspaces
  '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10")
  "Configured AeroSpace workspaces, in left-to-right order.
Unused workspace slots need not appear in AeroSpace's live workspace list."
  :type '(repeat string))

(defcustom thb-open-aerospace-program "aerospace"
  "AeroSpace executable used for window placement and focus."
  :type 'string)

(defvar thb-open--busy nil
  "Non-nil while an open request is being handled.")

(defun thb-open--aerospace (&rest args)
  "Run AeroSpace with separate ARGS; return its standard output.
No shell interprets arguments.  Signal an error on failure."
  ;; A previous request may have left Dired's default-directory on TRAMP.
  ;; AeroSpace always runs locally, independent of the buffer being viewed.
  (let ((default-directory "/"))
    (with-temp-buffer
      (let ((process (make-process
                      :name "thb-open-aerospace" :buffer (current-buffer)
                      :command (cons thb-open-aerospace-program args)
                      :connection-type 'pipe :noquery t :sentinel #'ignore)))
        (unwind-protect
            (progn
              ;; Keep servicing events while Accessibility asks the active
              ;; Emacs frame for its title; call-process can deadlock here.
              (with-timeout (5 (error "AeroSpace command timed out"))
                (while (process-live-p process)
                  (accept-process-output process 0.05)))
              (unless (and (eq (process-status process) 'exit)
                           (zerop (process-exit-status process)))
                (error "AeroSpace failed (%s): %s"
                       (process-exit-status process) (string-trim (buffer-string))))
              (buffer-string))
          (when (process-live-p process) (delete-process process)))))))

(defun thb-open--json (&rest args)
  "Run AeroSpace with ARGS and parse the JSON array response."
  (let ((result (json-parse-string
                 (apply #'thb-open--aerospace args)
                 :object-type 'alist :array-type 'array)))
    (unless (vectorp result)
      (error "Unexpected AeroSpace response"))
    (append result nil)))

(defun thb-open--normal-frame-p (frame)
  "Whether FRAME is a normal GUI frame suitable for editing."
  (and (frame-live-p frame)
       (display-graphic-p frame)
       (not (frame-parent frame))
       (not (eq (frame-parameter frame 'minibuffer) 'only))
       (not (frame-parameter frame 'no-accept-focus))
       (not (frame-parameter frame 'no-other-frame))))

(defun thb-open--existing-frame ()
  "Return a normal GUI frame, preferring the last selected one."
  (if (thb-open--normal-frame-p (selected-frame))
      (selected-frame)
    (let ((frames (seq-filter #'thb-open--normal-frame-p (frame-list))))
      (when frames
        ;; emacsclient evaluation may select the daemon's terminal frame.  Poll
        ;; a GUI display explicitly instead of that terminal's empty Z order.
        (or (seq-find #'thb-open--normal-frame-p
                      (frame-list-z-order (frame-parameter (car frames) 'display)))
            (car frames))))))

(defun thb-open--empty-workspace ()
  "Return the right-most configured workspace with no windows.
Count windows across all monitors; do not rely on existing workspace slots."
  (let* ((windows (thb-open--json "list-windows" "--all"
                                 "--format" "%{workspace}" "--json"))
         (occupied (mapcar
                    (lambda (window)
                      (or (alist-get 'workspace window)
                          (error "AeroSpace window has no workspace")))
                    windows)))
    (or (seq-find (lambda (workspace) (not (member workspace occupied)))
                  (reverse thb-open-workspaces))
        (user-error "No empty configured AeroSpace workspace; no frame created"))))

(defun thb-open--window-id (frame)
  "Resolve FRAME to its AeroSpace window ID without guessing from focus.
Emacs's window-id frame parameter is not an AeroSpace window ID on NS.
Temporarily give this frame a unique title, then restore its original title."
  (let ((old-title (frame-parameter frame 'title))
        (token (symbol-name (gensym "thbemacs-open-")))
        found)
    (unwind-protect
        (progn
          (set-frame-parameter frame 'title token)
          (dotimes (_ 20)
            (unless found
              (redisplay t)
              (let ((matches
                     (seq-filter
                      (lambda (window)
                        (equal (alist-get 'window-title window) token))
                      (thb-open--json "list-windows" "--monitor" "all"
                                      "--app-bundle-id" "org.gnu.Emacs" "--json"))))
                (when (> (length matches) 1)
                  (error "Multiple Emacs windows match the open request"))
                (setq found (alist-get 'window-id (car matches))))
              (unless found (sit-for 0.05))))
          (unless (and (integerp found) (> found 0))
            (error "Could not identify the Emacs window; no window moved"))
          (number-to-string found))
      (set-frame-parameter frame 'title old-title))))

(defun thb-open--focus-frame ()
  "Select a normal GUI frame, creating and placing one if needed.
Return (FRAME . AEROSPACE-WINDOW-ID).  Never move a pre-existing frame."
  (let* ((frame (thb-open--existing-frame))
         (workspace (unless frame (thb-open--empty-workspace))))
    (unless frame
      (setq frame (make-frame '((window-system . ns)
                                (name . "thbemacs") (minibuffer . t)))))
    (let ((window-id (thb-open--window-id frame)))
      (when workspace
        (thb-open--aerospace "move-node-to-workspace" "--window-id"
                             window-id "--" workspace))
      (thb-open--aerospace "focus" "--window-id" window-id)
      (select-frame-set-input-focus frame)
      (cons frame window-id))))

(defun thb-open--focus-final (window-id)
  "Asynchronously focus WINDOW-ID after loading and selecting the buffer.
Reuse the ID verified before opening.  Do not synchronously query AeroSpace
from the now-active Emacs frame: its Accessibility requests may need Emacs's
event loop.  Bound the subprocess lifetime and report failure without retry."
  (let ((default-directory "/") timer timed-out)
    (let ((process
           (make-process
            :name "thb-open-focus" :buffer nil :noquery t :connection-type 'pipe
            :command (list thb-open-aerospace-program "focus" "--window-id" window-id)
            :sentinel
            (lambda (process _event)
              (when (memq (process-status process) '(exit signal))
                (when timer (cancel-timer timer))
                (unless (or timed-out (and (eq (process-status process) 'exit)
                                          (zerop (process-exit-status process))))
                  (message "Opened the Emacs buffer, but AeroSpace focus failed (exit %s)"
                           (process-exit-status process))))))))
      (setq timer
            (run-at-time
             5 nil
             (lambda ()
               (when (process-live-p process)
                 (setq timed-out t)
                 (delete-process process)
                 (message "Opened the Emacs buffer, but AeroSpace focus timed out")))))
      process)))

;;;###autoload
(defun thb-open (&optional target directory)
  "Open TARGET in a focused GUI frame, or pick a file/directory.
DIRECTORY is the picker's starting directory and the base for relative TARGET.
Both accept ordinary local or TRAMP filenames.  Existing modified buffers are
selected without reverting them.  No daemon, transfer, or URL handler is set up."
  (interactive)
  (when thb-open--busy
    (user-error "An Emacs open request is already active"))
  (let* ((thb-open--busy t)
         (destination (thb-open--focus-frame))
         (frame (car destination))
         (window-id (cdr destination)))
    (let* ((default-directory (file-name-as-directory
                               (expand-file-name (or directory default-directory))))
           (path (expand-file-name
                  (or target
                      (read-file-name "Open file/directory: "
                                      default-directory nil t)))))
      (unless (file-exists-p path)
        (user-error "File or directory does not exist: %s" path))
      (if (file-directory-p path)
          (dired path)
        (find-file path))
      (let ((buffer (current-buffer)))
        (select-frame-set-input-focus frame)
        (switch-to-buffer buffer)
        ;; TRAMP/Dired may wait after the initial focus.  Emacs frame selection
        ;; alone does not reliably activate its macOS workspace/application.
        ;; Make OS focus the final step, after the result buffer is selected.
        (thb-open--focus-final window-id)
        buffer))))

(provide 'thb-open)
;;; thb-open.el ends here
