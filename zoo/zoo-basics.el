
;; Always show the matching parenthesis
(show-paren-mode 1)

;; Blink the cursor for easy tracking
(blink-cursor-mode 1)

;; Number the windows for easy access (alt-#)
(window-numbering-mode 1)

;; Keep track of windows layouts, to easily get
;; back to the previous one
(winner-mode 1)

;; Always have off menu-bar.
(menu-bar-mode 0)

;; Always reload files when changed.
(global-auto-revert-mode t)

;; Accept utf-8 characters on the terminal.
(set-terminal-coding-system 'utf-8-unix)

;; Don't want to write yes for everything.
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)

;; Don't make backup files.
(setq make-backup-files nil)
(auto-save-mode 0)

;; Disable keys that make *me* slower, but can't stop using them.
(put 'list-buffers 'disabled "Force yourself to use 'C-x b' instead")

;; Default folder for ephemeral content
(defvar zoo-ephemeral-dir "~/.emacs.ephemeral")

;; Keep all backup files in ephemeral
(setq backup-directory-alist '((".*" . ,zoo-ephemeral-dir)))


(provide 'zoo-basics)
