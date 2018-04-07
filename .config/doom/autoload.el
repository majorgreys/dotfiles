;;;  -*- lexical-binding: t; -*-

(require 'avy)

;;;###autoload
(defun ace-link--elfeed-collect ()
  "Collect the positions of visible links in `elfeed' buffer."
  (let (candidates pt)
    (save-excursion
      (save-restriction
        (narrow-to-region
         (window-start)
         (window-end))
        (goto-char (point-min))
        (setq pt (point))
        (while (progn (shr-next-link)
                      (> (point) pt))
          (setq pt (point))
          (when (plist-get (text-properties-at (point)) 'shr-url)
            (push (point) candidates)))
        (nreverse candidates)))))

;;;###autoload
(defun ace-link--elfeed-action  (pt)
  (goto-char pt)
  (shr-browse-url))

;;;###autoload
(defun ace-link-elfeed ()
  "Open a visible link in `elfeed' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-elfeed
              (avy--process
               (ace-link--elfeed-collect)
               #'avy--overlay-pre))))
    (ace-link--elfeed-action pt)))
