(require 'zoo-org)

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
       ((and (integerp evt)
             (char-equal evt ?k))
          ;; remove the j character
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
       (t ; otherwise
          (setq unread-command-events (append unread-command-events
                                              (list evt))))))))

;;;;;;;;;;;;;;
;; Insert Mode
;;;;;;;;;;;;;;
;;
;; Adding the binding for the j character, then
;; the k is handled on the function
(define-key evil-insert-state-map "j" #'zoo/jk-to-normal-mode)

;;;;;;;;;;;;;;
;; Normal Mode
;;;;;;;;;;;;;;
;;
;; Undo visualize tree on ",u" when on normal mode
(define-key evil-normal-state-map (kbd ",u") #'undo-tree-visualize)
;; Find defined symbols using ,. in normal mode instead of M-. in
;; insert mode
(define-key evil-normal-state-map (kbd ",.") 'find-tag)

;;;;;;;;;;;;;;;;;
;; Custom highlights for insert and normal mode
;; in the modeline
;;;;;;;;;;;;;;;;;
;;
;; We don't want evil's default for evil-mode-line-format,
;; so we are going to append it ourselves at the
;; start of the mode-line-format list
(setq evil-mode-line-format nil)
(setq zoo-mode-line-clock-in-tag t)

(defun zoo/evil-refresh-mode-line ()
  (interactive)
  (add-to-list 'mode-line-format " ")
  (when (and (boundp 'zoo-mode-line-clock-in-tag)
             (not (zoo/org-clocking-p)))
    (add-to-list 'mode-line-format
                 '(:eval zoo-mode-line-clock-in-tag)))
  (add-to-list 'mode-line-format
               '(:eval evil-mode-line-tag))
  (force-mode-line-update))

(defun zoo/highlight-org-clock ()
  (interactive)
  (if (and (boundp 'zoo-mode-line-clock-in-tag)
           zoo-mode-line-clock-in-tag
           (not (zoo/org-clocking-p)))
      (setq zoo-mode-line-clock-in-tag
            (propertize " CLOCK-IN "
                        'face
                        '(:background "#FF3366"
                          :foreground "white"
                          'bold)))
    (when zoo-mode-line-clock-in-tag
        ;; ^ set it to empty if it is actually defined
        (setq zoo-mode-line-clock-in-tag ""))))

(defmacro zoo/make-evil-highlight-mode (evil-mode
                                        modeline-foreground
                                        modeline-background
                                        evil-tag-foreground
                                        evil-tag-background
                                        evil-tag-text)
  (let ((function-name  (intern (format "zoo/highlight-%s-mode" evil-mode)))
        (tag-var-name   (intern (format "evil-%s-state-tag" evil-mode)))
        (evil-hook-name (intern (format "evil-%s-state-entry-hook"
                                        evil-mode))))
    `(progn
       ;; Create a function that will specify the
       ;; highlight for a evil-mode
       (defun ,function-name ()
         (interactive)
         (set-face-foreground 'modeline ,modeline-foreground)
         (set-face-background 'modeline ,modeline-background)
         (setq ,tag-var-name
               (propertize ,evil-tag-text
                           'face
                           '(:background ,evil-tag-background
                             :foreground ,evil-tag-foreground
                           'bold))))
       ;; Call the function for the simple initialization
       (,function-name)
       ;; Add a hook for the mode entry
       (add-hook ',evil-hook-name
                 ',function-name)
       ;; Add a hook to highlight the clock
       (add-hook ',evil-hook-name
                 'zoo/highlight-org-clock))))


(zoo/make-evil-highlight-mode "insert"    ; mode name
                              "white"     ; modeline foreground
                              "#0087AF"   ; modeline background
                              "#005F87"   ; mode foreground
                              "white"     ; mode background
                              " I ")      ; mode tag

(zoo/make-evil-highlight-mode "normal"
                              "black"
                              "#E6E5E4"
                              "#005F00"
                              "#AFD700"
                              " N ")

(zoo/make-evil-highlight-mode "emacs"
                              "black"
                              "#E6E5E4"
                              "white"
                              "#5F005F"
                              " E ")

(zoo/make-evil-highlight-mode "visual"
                              "black"
                              "#E6E5E4"
                              "#870000"
                              "#FF8700"
                              " VISUAL ")

(zoo/make-evil-highlight-mode "replace"
                              "black"
                              "#E6E5E4"
                              "white"
                              "#D70000"
                              " REPLACE ")

(zoo/make-evil-highlight-mode "motion"
                              "black"
                              "#E6E5E4"
                              "white"
                              "#14AACC"
                              " MOTION ")

(add-hook 'org-clock-in-hook   'zoo/highlight-org-clock)
(add-hook 'org-clock-in-hook   'zoo/evil-refresh-mode-line)

(add-hook 'org-clock-out-hook  'zoo/highlight-org-clock)
(add-hook 'org-clock-out-hook  'zoo/evil-refresh-mode-line)

(global-set-key (kbd "<f7> e") 'evil-emacs-state)
(global-set-key (kbd "<f7> n") 'evil-normal-state)

(defadvice evil-refresh-mode-line
  (before zoo-evil-refresh-mode-line activate)
  (zoo/evil-refresh-mode-line))

(defadvice evil-mode-enable-in-buffers
  (after zoo-evil-mode-enable-in-buffers activate)
  (zoo/evil-refresh-mode-line))

(evil-mode 1)
(global-surround-mode 1)

(provide 'zoo-evil)
