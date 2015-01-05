;;; package --- Summary
;;; Code:
;;; Commentary:
(add-to-list 'load-path (expand-file-name "~/.emacs.d/zoo"))

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Allow package-install to add entries to Cask file
(require 'pallet)
(pallet-mode 1)

(require 'birdseye)
(require 'zoo-basics)
(require 'zoo-keybinding)
(require 'zoo-rainbow-delimiters)
(require 'zoo-paredit)
(require 'zoo-elisp)
(require 'zoo-ido)
(require 'zoo-term)
(require 'zoo-evil)
(require 'zoo-theme)
(require 'zoo-org)
(require 'zoo-bookmarks)
(require 'zoo-whitespace)
(require 'zoo-ruby)
(require 'zoo-clojure)
(require 'zoo-recentf-history-etc)
(require 'zoo-find)
(require 'zoo-ibuffer)
(require 'zoo-powerline)
(require 'dss-hook-management)

(provide 'init)

;;; init.el ends here
