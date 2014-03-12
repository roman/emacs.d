(require 'multi-term)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Settings for terminal behavior

(setq system-uses-terminfo nil)
(setq comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output nil
      comint-scroll-show-maximum-output t)

(setq shell-command-switch "-lc")

(defun zoo/toggle-term-mode ()
  (interactive)
  (cond
   ((term-in-line-mode)
    (progn
      (term-char-mode)
      (evil-emacs-state)
      (setq zoo/current-term-mode 'term-char-mode)
      (comint-goto-process-mark)))
   ((term-in-char-mode)
    (progn
      (term-line-mode)
      (evil-normal-state)
      (setq zoo/current-term-mode 'term-line-mode)))
   (t
    (progn
      (message
       (concat "Invalid value for `zoo/current-term-mode'"
               ", setting it to 'term-char-mode'"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Extensions for terminal

(define-minor-mode evil-term-mode
  "Evil minor mode for multiterm"
  :keymap (make-sparse-keymap))

(evil-set-initial-state 'term-mode 'emacs)

(evil-define-key 'normal 'evil-term-mode-map
  (kbd "M-g") 'zoo/toggle-term-mode)

(evil-define-key 'emacs evil-term-mode-map
  (kbd "C-r") 'term-send-reverse-search-history
  (kbd "<f1>") 'zoo/toggle-term-mode
  (kbd "M-g") 'zoo/toggle-term-mode)

(add-hook 'term-mode-hook 'evil-term-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(provide 'zoo-term)
