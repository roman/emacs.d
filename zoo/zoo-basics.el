; always have on ido mode
(ido-mode 1)

; always have off menu-bar
(menu-bar-mode 0)

; accept utf-8 characters on the terminal
(set-terminal-coding-system 'utf-8-unix)

; on shells, please handle properly the ansi escape codes
(add-hook 'shell-mode-hook  'ansi-color-for-comint-mode-on)

; disable keys that make *me* slower, but can't stop using them
(put 'list-buffers 'disabled "Force yourself to use 'C-x b' instead")

(provide 'zoo-basics)
