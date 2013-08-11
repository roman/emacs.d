(require 'golden-ratio)
(require 'uniquify)


;; Always show the matching parenthesis
(show-paren-mode 1)

;; Blink the cursor for easy tracking
(blink-cursor-mode 1)

;; Number the windows for easy access (alt-#)
(window-numbering-mode 1)

;; Show both line and column number
(column-number-mode 1)

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

;; I want flymake to show me errors
(setq flymake-log-level 1)

;; Disable keys that make *me* slower, but can't stop using them.
(put 'list-buffers 'disabled "Force yourself to use 'C-x b' instead")

;; Default folder for ephemeral content
(defvar zoo-ephemeral-dir "~/.emacs.ephemeral/")

;; Keep all backup files in ephemeral
(defvar user-temporary-file-directory (concat zoo-ephemeral-dir "tmp/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist `(("." . ,user-temporary-file-directory)))

;; Give me a space on the right for line numbers
(setq linum-format "%3d ")

;; When having windows with repeated filenames, uniquify them
;; by the folder they are in rather those annoying <2>,<3>,.. etc
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
; don't screw special buffers
(setq uniquify-ignore-buffers-re "^\\*")

(setq notify-delay '(0 0 0))

;; Enable auto-resizing of windows with golden-ratio

(setq golden-ratio-extra-commands
      (append golden-ratio-extra-commands
              '(evil-window-left
                evil-window-right
                evil-window-up
                evil-window-down)))
(golden-ratio-mode)


(provide 'zoo-basics)
