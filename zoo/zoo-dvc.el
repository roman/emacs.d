(require 'evil)

(defun zoo/magit-mode ()
  (interactive)
  (evil-define-key 'motion magit-status-mode-map
    (kbd "RET") 'magit-visit-item))

(defun zoo/dvc-mode ()
  (interactive)
  (evil-define-key 'motion diff-mode-map
    (kbd "q") 'quit-window
    (kbd "i") 'diff-goto-source))

(evil-set-initial-state 'diff-mode 'motion)
(evil-set-initial-state 'magit-status-mode 'motion)

(add-hook 'magit-status-mode-hook 'zoo/magit-mode)
(add-hook 'dvc-mode-hook 'zoo/dvc-mode)

(provide 'zoo-dvc)