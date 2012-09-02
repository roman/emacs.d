;;;;;;;;;;;;;;;;;
;; default modes always on
;;;;;;;;;;;;;;;;;

(evil-mode 1)
(global-surround-mode 1)

;;;;;;;;;;;;;;;;;
;; jk binding
;;;;;;;;;;;;;;;;;

; In this section we implement code that will allow us
; to get into evil-normal mode using "jk" in insert mode.
;
(evil-define-command zoo/jk-to-normal-mode ()
  "Allows to get into 'normal' mode using 'jk'."
  :repeat change
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
                           nil 0.5)))
      (cond
       ((null evt)
          (message ""))
       ((and (integerp evt) (char-equal evt ?k))
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
       (t ; otherwise
          (setq unread-command-events (append unread-command-events
                                              (list evt))))))))

; Adding the binding for the j character, then
; the k is handled on the function
(define-key
  evil-insert-state-map
  "j"
  #'zoo/jk-to-normal-mode)



;;;;;;;;;;;;;;;;;
;; ESC Warning
;;;;;;;;;;;;;;;;;

;(evil-define-command zoo/esc-warning (arg)
;  "Wait for further keys within `evil-esc-delay'.
;   Otherwise send [escape]."
;  :repeat ignore
;  (interactive "P")
;  (if (sit-for evil-esc-delay t)
;      (progn
;        (push 'escape unread-command-events)
;        (when defining-kbd-macro
;          ;; we need to replace the ESC by 'escape in the currently
;          ;; defined keyboard macro
;          (evil-save-echo-area
;            (end-kbd-macro)
;            (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
;            (start-kbd-macro t t))))
;    (push last-command-event unread-command-events)
;    ;; preserve prefix argument
;    (setq prefix-arg arg))
;  ;; disable interception for the next key sequence
;  (message "you should use 'jk' instead of ESC")
;  (evil-esc-mode -1)
;  (setq this-command last-command))
;
;(define-key
;  evil-esc-map
;  (kbd "ESC")
;  #'zoo/esc-warning)

;;;;;;;;;;;;;;;;;
;; ESC extras
;;;;;;;;;;;;;;;;;

; Make <ESC> quit almost everything...
; As seen on:
; * http://stackoverflow.com/questions/8483182/emacs-evil-mode-best-practice
;(define-key evil-normal-state-map [escape] 'keyboard-quit)
;(define-key evil-visual-state-map [escape] 'keyboard-quit)
;(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


;;;;;;;;;;;;;;;;;
;; Custom highlights for insert and normal mode
;;;;;;;;;;;;;;;;;

(defun zoo/highlight-insert-mode ()
  (interactive)
  (if (and (fboundp 'org-clocking-p)
           (org-clocking-p))
    (progn
      (set-face-foreground 'modeline "white")
      (set-face-background 'modeline "#0087af"))
    (progn
      (set-face-foreground 'modeline "black")
      (set-face-background 'modeline "#FF3366"))))

(defun zoo/highlight-normal-mode ()
  (interactive)
  (set-face-foreground 'modeline "black")
  (set-face-background 'modeline "#e6e5e4"))

(add-hook 'evil-insert-state-entry-hook  'zoo/highlight-insert-mode)
(add-hook 'evil-insert-state-exit-hook 'zoo/highlight-normal-mode)

(provide 'zoo-evil)
