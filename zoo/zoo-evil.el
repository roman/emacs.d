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
(evil-define-key 'normal global-state-map
  (kbd ",u") #'undo-tree-visualize)

;;
;; When using '*' or '#' search for symbols and
;; not words
(setq-default evil-symbol-word-search t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zoo/evil-roman-sitdown ()
  (interactive)

  (setq evil-default-state 'normal)
  (evil-set-initial-state 'term-mode 'normal)
  (evil-set-initial-state 'org-mode 'normal)

  (dolist (buffer (buffer-list))
    (when (not (let ((case-fold-search nil))
                 (string-match "Minibuf" (buffer-name buffer))))
      (with-current-buffer buffer
        (cond
         ((evil-emacs-state-p) (evil-normal-state)))))))

(defun zoo/evil-tavis-sitdown ()
  (interactive)

  (setq evil-default-state 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'org-mode 'emacs)

  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (cond
       ((not (evil-emacs-state-p)) (evil-emacs-state))))))

(defun tavis-jack-in ()
  (interactive)
  (zoo/evil-tavis-sitdown))

(defun roman-jack-in ()
  (interactive)
  (zoo/evil-roman-sitdown))

(global-set-key (kbd "<f7> t") 'tavis-jack-in)
(global-set-key (kbd "<f7> r") 'roman-jack-in)

(evil-mode 1)
;; (global-surround-mode 1)

(provide 'zoo-evil)
