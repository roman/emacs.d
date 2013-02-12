(require 'multi-term)
;;;;;;;;;;;;;;;;;;;;
;; Evil Extensions for terminal
;;;;;;;;;;;;;;;;;;;;

(define-minor-mode evil-term-mode
  "Evil minor mode for multiterm"
  :keymap (make-sparse-keymap))

(evil-set-initial-state 'term-mode 'emacs)
(evil-define-key 'insert evil-term-mode-map
  (kbd "C-r") 'term-send-reverse-search-history)

(add-hook 'term-mode-hook 'evil-term-mode)

;;;;;;;;;;;;;;;;;;;;

; on shells, please handle properly the ansi escape codes
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq multi-term-program "/bin/bash")

; don't switch to other multi-term when closing
; the current one
(setq multi-term-switch-after-close nil)

;; Following code was take from:
;; http://emacswiki.org/emacs/MultiTerm
(defun zoo/last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (zoo/last-term-buffer (cdr l)))))

(defun zoo/get-term ()
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (zoo/last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
        (multi-term)
      (switch-to-buffer b))))

;; (setq multi-term-ext-profiles
;;       '(("gentoo" . ((multi-term-ext-remote-host "vagrant@127.0.0.1")
;;                      (multi-term-ext-remote-ssh-port "2222")
;;                      (multi-term-ext-screen-session-name "emacs")
;;                      (multi-term-buffer-name "vagrant")))
;;         ("ateam" . ((multi-term-ext-remote-host "roman@ateam")
;;                     (multi-term-ext-screen-session-name "emacs")
;;                     (multi-term-buffer-name "ateam")))
;;         ("irb-local" . ((multi-term-program "/usr/bin/irb")
;;                         (multi-term-buffer-name "irb-local")
;;                         (multi-term-ext-screen-session-name "irb")))))

(provide 'zoo-term)
