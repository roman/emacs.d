(defvar f4-map
  (make-sparse-keymap)
  "Keybinding map that is going to be used for lang-mode specifics")
(define-key global-map [(f4)] f4-map)

(defvar f8-map
  (make-sparse-keymap)
  "Keybinding map use only for org mode commands")
(define-key global-map [(f8)] f8-map)

(define-key global-map
  (kbd "C-x C-s") 'zoo/save-buffer)

(provide 'zoo-keybinding)
