(defun zoo.shell/local-shell-to-str (command)
  (with-temp-buffer
    (shell-command-to-string command)))

(provide 'zoo.shell)