;; the tab-width is 4 by default
(setq tab-width 4)

;; don't highlight lines with 8 spaces or more at the start
(setq-default indent-tabs-mode nil)

;; whitespace-mode should only show this properties
(setq whitespace-style
      '(face
        tabs
        spaces
        trailing
        newline
        ;; newline-mark
        empty
        space-before-tab
        space-after-tab))

;; special characters for newline and tabs
(setq whitespace-display-mappings
      '(
        (newline-mark 10 [194 172 10])
        (tab-mark 9 [226 150 184 9])
        ))

;; special colors for newline and tab character
(custom-set-faces
 '(whitespace-space
   ((((class color) (background dark))
     (:background "#111" :foreground "white"))
    (((class color) (background light))
     (:background "yellow" :foreground "black"))
    (t (:inverse-video t))))
 '(whitespace-newline
   ((((class color) (background dark))
     (:background "#111" :foreground "#111"))))
 '(whitespace-tab
   ((((class color) (background dark))
     (:background "#111" :foreground "#111")))))

;; f4 should be whitespace-cleanup
(global-set-key (kbd "<f4> SPC") 'whitespace-cleanup)
(define-key evil-normal-state-map
  (kbd ", SPC") 'whitespace-cleanup)

(add-hook 'after-save-hook 'whitespace-cleanup)

;; whitespace always on
;; fucks up screen
(global-whitespace-mode 1)

(provide 'zoo-whitespace)
