(require 'paredit nil)
(require 'evil-paredit)

(defun zoo/turn-on-paredit ()
  (interactive)
  (paredit-mode 1)
  (evil-paredit-mode 1))

(defun zoo/next-sexp-on-line? ()
  (interactive)
  (condition-case nil
      (or
       (save-excursion
         (evil-find-char 1 ?\()
         t)
       (looking-at "\("))
    (error
     nil)))

(evil-define-key 'normal paredit-mode-map
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

(defun zoo/next-sexp-on-line? ()
  (interactive)
  (condition-case nil
      (save-excursion
        (evil-find-char 1 ?\()
        t)
    (error
     nil)))

(provide 'zoo-paredit)
