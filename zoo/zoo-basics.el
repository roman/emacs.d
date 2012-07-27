(ido-mode 1)
(show-paren-mode 1)
(blink-cursor-mode 1)

; always have off menu-bar
(menu-bar-mode 0)

; accept utf-8 characters on the terminal
(set-terminal-coding-system 'utf-8-unix)

; don't want to write yes
(fset 'yes-or-no 'y-or-n)

; don't make backup files
(setq make-backup-files nil)


; disable keys that make *me* slower, but can't stop using them
(put 'list-buffers 'disabled "Force yourself to use 'C-x b' instead")

(provide 'zoo-basics)
