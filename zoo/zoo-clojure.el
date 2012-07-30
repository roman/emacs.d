(require 'paredit)
(require 'zoo-paredit)
(require 'zoo-rainbow-delimiters)

(add-hook 'clojure-mode-hook 'zoo/turn-on-paredit)
(add-hook 'clojure-mode-hook 'zoo/turn-on-rainbow-delimiters)

(provide 'zoo-clojure)
