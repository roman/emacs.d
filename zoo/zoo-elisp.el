(require 'zoo-paredit)
(require 'zoo-rainbow-delimiters)

(defun zoo/emacs-lisp-mode-hook ()
  (interactive)
  (zoo/turn-on-paredit)
  (zoo/turn-on-rainbow-delimiters)
  (eldoc-mode 1))

(add-hook 'emacs-lisp-mode-hook 'zoo/emacs-lisp-mode-hook)

(provide 'zoo-elisp)
