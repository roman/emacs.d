;;; zoo --- Summary here
;;; Commentary:
;;; Code:
(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq markdown-command "pandoc --smart -f markdown -t html")

(provide 'zoo-markdown)

;;; zoo-markdown ends here
