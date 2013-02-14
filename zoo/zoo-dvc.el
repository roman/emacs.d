(require 'magit)
(require 'evil)

;; Currently not installing dvc
;; because it requires mercurial? WTF?
(defun zoo/dvc-status ()
   (interactive)
   ;; (if (magit-git-repo-p))
   (magit-status))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(evil-define-key 'motion magit-status-mode-map
     (kbd "RET") 'magit-visit-item)

(evil-define-key 'motion diff-mode-map
     (kbd "q") 'quit-window
     (kbd "i") 'diff-goto-source)

(evil-define-key 'normal global-map
  (kbd ",vs") 'zoo/dvc-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(evil-set-initial-state 'diff-mode 'motion)
(evil-set-initial-state 'magit-status-mode 'motion)

(provide 'zoo-dvc)
