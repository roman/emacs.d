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
;; in the modeline
;;;;;;;;;;;;;;;;;

(defun zoo/org-clocking-p ()
  (interactive)
  (and (fboundp 'org-clocking-p)
       (org-clocking-p)))

;; We don't want evil's default for evil-mode-line-format,
;; so we are going to append it ourselves at the
;; start of the mode-line-format list
(setq evil-mode-line-format nil)
(setq mode-line-clock-in-tag t)
(defadvice evil-refresh-mode-line (after zoo-evil-refresh-mode-line activate)
  (add-to-list 'mode-line-format " ")
  (when (and (boundp 'mode-line-clock-in-tag)
             (not (zoo/org-clocking-p)))
    (add-to-list 'mode-line-format '(:eval mode-line-clock-in-tag)))
  (add-to-list 'mode-line-format
               '(:eval evil-mode-line-tag))
  (force-mode-line-update))

(defun zoo/highlight-clock-in ()
  (interactive)
  (if (and (boundp 'mode-line-clock-in-tag)
           mode-line-clock-in-tag
           (not (zoo/org-clocking-p)))
      (setq mode-line-clock-in-tag
            (propertize " CLOCK-IN "
                        'face
                        '(:background "#FF3366"
                          :foreground "white"
                          'bold)))
    (when mode-line-clock-in-tag
        (setq mode-line-clock-in-tag ""))))

(defun zoo/highlight-insert-mode ()
  (interactive)
  (set-face-foreground 'modeline "white")
  (set-face-background 'modeline "#0087AF")
  (setq evil-insert-state-tag
        (propertize " I "
                    'face
                    '(:background "white"
                      :foreground "#005F87"
                      'bold))))

(defun zoo/highlight-emacs-mode ()
  (interactive)
  ;; modeline
  (set-face-foreground 'modeline "black")
  (set-face-background 'modeline "#E6E5E4")
  ;; emacs state tag
  (setq evil-emacs-state-tag
        (propertize " E "
                    'face
                    '(:background "#5F005F"
                      :foreground "white"
                      'bold))))

(defun zoo/highlight-normal-mode ()
  (interactive)
  ;; modeline
  (set-face-foreground 'modeline "black")
  (set-face-background 'modeline "#E6E5E4")
  ;; normal state tag
  (setq evil-normal-state-tag
        (propertize " N "
                    'face
                    '(:background "#AFD700"
                      :foreground "#005F00"
                      'bold))))

(defun zoo/highlight-visual-mode ()
  (interactive)
  (set-face-foreground 'modeline "black")
  (set-face-background 'modeline "#E6E5E4")
  (setq evil-visual-state-tag
        (propertize " V "
                    'face
                    '(:background "#FF8700"
                      :foreground "#870000"
                      'bold))))

(defun zoo/highlight-replace-mode ()
  (interactive)
  (set-face-foreground 'modeline "black")
  (set-face-background 'modeline "#E6E5E4")
  (setq evil-replace-state-tag
        (propertize " R "
                    'face
                    '(:background "#D70000"
                      :foreground "white"
                      'bold))))

(add-hook 'evil-insert-state-entry-hook  'zoo/highlight-insert-mode)
(add-hook 'evil-insert-state-entry-hook  'zoo/highlight-clock-in)

(add-hook 'evil-emacs-state-entry-hook   'zoo/highlight-emacs-mode)
(add-hook 'evil-emacs-state-entry-hook   'zoo/highlight-clock-in)

(add-hook 'evil-normal-state-entry-hook  'zoo/highlight-normal-mode)
(add-hook 'evil-normal-state-entry-hook  'zoo/highlight-clock-in)

(add-hook 'evil-visual-state-entry-hook  'zoo/highlight-visual-mode)
(add-hook 'evil-visual-state-entry-hook  'zoo/highlight-clock-in)

(add-hook 'evil-replace-state-entry-hook  'zoo/highlight-replace-mode)
(add-hook 'evil-replace-state-entry-hook  'zoo/highlight-clock-in)

(global-set-key (kbd "<f7> e") 'evil-emacs-state)
(global-set-key (kbd "<f7> n") 'evil-normal-state)

(zoo/highlight-insert-mode)
(zoo/highlight-normal-mode)
(zoo/highlight-emacs-mode)
(evil-mode 1)
(surround-mode 1)

(provide 'zoo-evil)
