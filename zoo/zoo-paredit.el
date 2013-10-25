(require 'paredit nil)
(require 'evil-paredit)

(defun zoo/turn-on-paredit ()
  (interactive)
  (paredit-mode 1)
  (evil-paredit-mode 1))

(define-skeleton dss/elisp-let-skeleton
    "A simple e-lisp let skeleton"
      nil
        "(let ((" @ - "))" \n >
          @ _ ")")

(setq dss-let-skeleton-func 'dss/elisp-let-skeleton)

(defun zoo/evil-paredit-build-let ()
  (interactive)
  (cond
   ((looking-back "[[:alnum:]-]") nil)
   ((and (not (dss/in-string-p))
         (looking-at-p "\("))
    (progn
      (if (not mark-active) (mark-sexp))
      (funcall dss-let-skeleton-func)
      (evil-insert-state)))))


(evil-define-key 'normal paredit-mode-map
  (kbd ",l")  'zoo/evil-paredit-build-let
  (kbd ",>")  'paredit-forward-slurp-sexp
  (kbd ",<")  'paredit-backward-slurp-sexp
  (kbd ";>")  'paredit-backward-barf-sexp
  (kbd ";<")  'paredit-forward-barf-sexp
  ;; (kbd ",xs")  'paredit-split-sexp
  ;; (kbd ",xj")  'paredit-join-sexps
  (kbd ",ks") 'paredit-splice-sexp
  (kbd ",kf") 'paredit-splice-sexp-killing-forward
  (kbd ",kb") 'paredit-splice-sexp-killing-backward
  (kbd ",ku") 'paredit-raise-sexp
  (kbd "(")   'paredit-backward
  (kbd ")")   'paredit-forward)

(provide 'zoo-paredit)
