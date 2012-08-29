(require 'erc)

(setq lexical-binding t)

(setq erc-log-channels-directory "~/.erc/logs/"
      erc-save-buffer-on-part t
      erc-log-write-after-insert t
      erc-log-write-after-send t
      erc-log-all-but-server-buffers t)

(define-key erc-mode-map (kbd "C-M-l") 'end-of-buffer)

(defun dss/erc-echo-file-save ()
  (interactive)
  (let ((line (format "--- Tavis saved  \"%s\" " (buffer-file-name))))
    (with-current-buffer "#git"
      (erc-send-message line))))

(defun dss/slime-repl-eval-string (string)
  (interactive)
  (slime-eval-async `(swank:eval-and-grab-output ,string)
                    (lambda (result)
                      (destructuring-bind (output value) result
                        (erc-send-message (format "%s => %S" string value))))))

(defun erc-cmd-CLJ (&rest form)
  "Eval FORM and send the result and the original form as:
FORM => (eval FORM)."
  (let* ((form-string (mapconcat 'identity form " "))
         (result
          (condition-case err
              (progn
                (dss/slime-repl-eval-string form-string))
              (error (format "Error: %s" err)))))))
