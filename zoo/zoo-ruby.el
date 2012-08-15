;; on irb like shells, please handle properly the ansi escape codes
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-hook 'ruby-mode-hook (lambda ()
                            (ruby-end-mode 1)
                            (set (make-local-variable 'tab-width) 2)))
(add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)



(provide 'zoo-ruby)
