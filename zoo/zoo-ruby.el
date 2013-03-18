;; on irb like shells, please handle properly the ansi escape codes
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-hook 'ruby-mode-hook (lambda ()
                            (ruby-end-mode 1)
                            (set (make-local-variable 'tab-width) 2)))
(add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)

(defun zoo/ruby-mode-hook ()
  ;;(make-local-variable 'tags-table-list)
  (setq tags-table-list
        (split-string  (shell-command-to-string
                        (format "ls %s"
                                (concat (getenv "GEM_HOME")
                                        "/gems/*/tags")))
                       "\n")))

(add-hook 'ruby-mode-hook 'zoo/ruby-mode-hook)

(provide 'zoo-ruby)
