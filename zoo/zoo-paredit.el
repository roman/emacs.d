(require 'evil nil)
(require 'surround nil)
(require 'paredit nil)

(defun zoo/turn-on-paredit ()
  (interactive)
  (paredit-mode 1))


(defun zoo/paredit-backward-edit (operation)
  (interactive
   (progn
     ;; abort the calling operator
     (setq evil-inhibit-operator t)
     (list (assoc-default evil-this-operator
                          '((evil-change . change)
                            (evil-delete . delete))))))
  (cond
   ((eq operation 'delete) (call-interactively 'paredit-backward-kill-word)
    ((eq operation 'change)
     (call-interactively 'paredit-backward-kill-word)
     (call-interactively 'evil-insert-state)))))

(defun zoo/paredit-forward-edit (operation)
  (interactive
   (progn
     ;; abort the calling operator
     (setq evil-inhibit-operator t)
     (list (assoc-default evil-this-operator
                          '((evil-change . change)
                            (evil-delete . delete))))))
  (cond
   ((eq operation 'delete) (call-interactively 'paredit-forward-kill-word))
   ((eq operation 'change)
      (call-interactively 'paredit-forward-kill-word)
      (call-interactively 'evil-insert-state))))

(evil-define-key 'normal paredit-mode-map
  (kbd ",>") 'paredit-forward-slurp-sexp
  (kbd ",<") 'paredit-backward-slurp-sexp
  (kbd ";>")  'paredit-backward-barf-sexp
  (kbd ";<")  'paredit-forward-barf-sexp
  (kbd ",s") 'paredit-split-sexp
  (kbd ",j") 'paredit-join-sexps
  (kbd ",ks") 'paredit-splice-sexp
  (kbd ",kf") 'paredit-splice-sexp-killing-forward
  (kbd ",kb") 'paredit-splice-sexp-killing-backward
  (kbd ",ku") 'paredit-raise-sexp
  (kbd "(")  'paredit-backward
  (kbd ")")  'paredit-forward
  (kbd "D")  'paredit-kill)

(evil-define-key 'operator paredit-mode-map
  "w" 'zoo/paredit-forward-edit
  "b" 'zoo/paredit-backward-edit)

(provide 'zoo-paredit)
