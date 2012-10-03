(require 'comint)
(provide 'remote-repl)


(defvar remote-repl-buffer "remote-repl")

(defvar remote-repl-clientpy
  (expand-file-name
   "client.py"
   (file-name-directory (or load-file-name (buffer-file-name)))))

(defun run-remote-repl (&optional dont-switch-p)
  (interactive)

  (setq remote-repl-buffer
        (make-comint "remote-repl" "python" nil remote-repl-clientpy))
  (if (not dont-switch-p)
      (pop-to-buffer remote-repl-buffer)))

(defun remote-repl-send-string (cmdstr)
   (comint-send-string remote-repl-buffer cmdstr)
   (comint-send-string remote-repl-buffer "\n"))

(defun remote-repl-send-region (&optional start end)
  (interactive "r")
  (let* ((cmdstr (buffer-substring-no-properties start end)))
    (remote-repl-send-string cmdstr)))
