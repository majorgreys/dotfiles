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
  (with-temp-buffer
    (let ((status (apply #'call-process thb-open-aerospace-program
                         nil t nil args)))
      (unless (eql status 0)
        (error "AeroSpace failed (%s): %s" status (string-trim (buffer-string))))
      (buffer-string))))

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
Return the selected frame.  Never move a pre-existing frame."
  (let* ((frame (thb-open--existing-frame))
         (workspace (unless frame (thb-open--empty-workspace))))
    (unless frame
      (setq frame (make-frame '((window-system . ns)
                                (name . "thbemacs") (minibuffer . t)))))
    (let ((window-id (thb-open--window-id frame)))
      (when workspace
        (thb-open--aerospace "move-node-to-workspace" "--window-id"
                             window-id "--" workspace))
      (thb-open--aerospace "focus" "--window-id" window-id))
    (select-frame-set-input-focus frame)
    frame))

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
         (frame (thb-open--focus-frame)))
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
        buffer))))

(provide 'thb-open)
;;; thb-open.el ends here
