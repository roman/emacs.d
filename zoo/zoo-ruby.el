;; on irb like shells, please handle properly the ansi escape codes
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'zoo-ruby)
