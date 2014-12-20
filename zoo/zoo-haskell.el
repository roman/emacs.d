;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'evil)
(require 'be-utils)
(require 'haskell-mode)
(require 'flycheck-haskell)


(be/util-eval-on-load ("flycheck" "haskell-mode")
  (flycheck-mode 1)
  (flycheck-haskell-setup))


(provide 'zoo-haskell)

;;; zoo-haskell ends here
