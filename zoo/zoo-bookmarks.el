(require 'bm)

;; Define keybindings for bookmarking lines for
;; both global-map and window-numbering-keymap
(mapc (lambda (map)
        (define-key map (kbd "M-8") 'bm-previous)
        (define-key map (kbd "M-9") 'bm-next)
        (define-key map (kbd "M-`") 'bm-toggle))
      (list global-map window-numbering-keymap))

(provide 'zoo-bookmarks)
