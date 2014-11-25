;; (require 'monokai-theme)
;; (require 'ujelly-theme)
;; (require 'moe-theme)
(require 'color-theme-sanityinc-tomorrow)

(custom-set-faces
 '(flycheck-info        ((t (:background "#444" :underline t :style wave))))
 '(flycheck-warning     ((t (:background "#7A6200" :inverse t :underline t))))
 '(flycheck-error       ((t (:background "#800000" :inverse t :underline t :bold t))))
 '(cider-error-highlight-face  ((t (:inverse-video t
                                    :background "#800000"
                                    :underline t
                                    :bold t))))
 '(magit-item-highlight ((t nil))))

;; (load-theme 'wombat)
;; (load-theme 'monokai t)
;; (load-theme 'ujelly t)
;; (load-theme 'moe-radr t)
(load-theme 'sanityinc-tomorrow-eighties t)

(provide 'zoo-theme)
