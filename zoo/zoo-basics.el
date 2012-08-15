(require 'zoo-rainbow-delimiters)
(require 'zoo-paredit)

(show-paren-mode 1)
(blink-cursor-mode 1)
(window-numbering-mode 1)
(winner-mode 1)

; always have off menu-bar
(menu-bar-mode 0)

; always reload files when changed
(global-auto-revert-mode t)

; accept utf-8 characters on the terminal
(set-terminal-coding-system 'utf-8-unix)

; don't want to write yes
(fset 'yes-or-no 'y-or-n)

; don't make backup files
(setq make-backup-files nil)
(auto-save-mode 0)

; disable keys that make *me* slower, but can't stop using them
(put 'list-buffers 'disabled "Force yourself to use 'C-x b' instead")

; Have rainbow delimiters on lisp
(add-hook 'emacs-lisp-mode-hook 'zoo/turn-on-rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook 'zoo/turn-on-paredit)


(provide 'zoo-basics)
