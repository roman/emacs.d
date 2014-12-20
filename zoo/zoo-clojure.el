(require 'paredit)
(require 'lineker)
(require 'be-utils)
(require 'zoo-paredit)
(require 'zoo-rainbow-delimiters)
(require 'zoo-keybinding)

;;;;;;;;;;;;;;;;;;;;

(be/util-eval-on-mode "clojure-mode"
  (setq clojure-test-ns-segment-position 1)

  (linum-mode 1)
  (paredit-mode 1)
  (evil-paredit-mode 1)
  (rainbow-delimiters-mode 1)

  (setq lineker-column-limit 90)
  (lineker-mode 1))

(provide 'zoo-clojure)
