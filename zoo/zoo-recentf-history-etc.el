(require 'zoo-basics)

;; Persist code bookmarks
(setq bookmark-save-flag 1)

;; Persist minibuffer history
(savehist-mode 1)
(setq savehist-file
      (concat zoo-ephemeral-dir "history"))

;; Append to the history also other rings
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))

;; Add support for saving bookmarks on different
;; emacs sessions
;; Ask Tavis: what's the difference btw bookmark and this?
(require 'saveplace)
(setq save-place-file
      (concat zoo-ephemeral-dir "saveplace"))
(setq-default save-place t)

;; Allow to lookup recent accessed files easily
(require 'recentf)
(recentf-load-list)
(recentf-mode 1)
(setq recentf-save-file
      (concat zoo-ephemeral-dir "recentf"))
(setq recentf-max-saved-items 150)
(setq recentf-max-menu-items 60)
;; Update the recent files list every 1200 seconds
(run-with-timer (* 2 60) (* 5 60) 'recentf-save-list)
(add-hook 'recentf-dialog-mode-hook
          (lambda ()
            (linum-mode +1)))

(defun dss/ido-choose-from-recentf ()
  ;;from http://www.xsteve.at/prg/emacs/power-user-tips.html
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string
                                     (concat home "/") "~/"
                                     path nil t))
                                  recentf-list)
                          nil t))))

(define-key global-map (kbd "C-x C-r") 'dss/ido-choose-from-recentf)

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/session")
;; (require 'session)
;; (add-hook 'after-init-hook 'session-initialize)
;; (setq session-save-file (concat zoo-ephemeral-dir "session"))
;; (setq session-save-file-coding-system 'utf-8)

(provide 'zoo-recentf-history-etc)
