;; on irb like shells, please handle properly the ansi escape codes
(add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'zoo-ruby)
