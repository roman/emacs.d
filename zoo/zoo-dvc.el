(require 'evil)

(defun zoo/dvc-mode ()
  (interactive)
  (evil-define-key 'normal diff-mode-map
    (kbd "q") 'quit-window
    (kbd "i") 'diff-goto-source))

(add-hook 'dvc-mode-hook 'zoo/dvc-mode)

(provide 'zoo-dvc)