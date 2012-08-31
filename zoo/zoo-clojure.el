(require 'paredit)
(require 'zoo-paredit)
(require 'zoo-rainbow-delimiters)

(defun zoo/clojure-mode-hook
  (interactive)
  (require 'clojure-test-mode)
  (setq clojure-test-ns-segment-position 1)
  (zoo/turn-on-paredit)
  (zoo/turn-on-rainbow-delimiters))

(add-hook 'clojure-mode-hook 'zoo/clojure-mode-hook)

(defun dss/clojure-run-tests ()
  (interactive)
  (save-window-excursion
    (if (not (dss/clojure-in-tests))
        (clojure-jump-to-test))
    (clojure-test-run-tests)))

(defun dss/slime-repl-clear ()
  (interactive)
  (save-window-excursion
    (slime-switch-to-output-buffer)
    (slime-repl-clear-buffer)
    (end-of-buffer)
    ;(dss/sync-point-all-windows)
    ))

(defun dss/clojure-jump-to-project ()
  "Jump to project.clj"
  (interactive)
  (find-file (format "%s/project.clj"
                     (locate-dominating-file buffer-file-name "src/"))))

(defun dss/clojure-in-tests ()
  (string-match "test" (clojure-find-ns)))

(defun dss/clojure-jump-between-tests-and-code ()
  (interactive)
  (if (dss/clojure-in-tests)
      (clojure-test-jump-to-implementation)
    (clojure-jump-to-test)))

(defvar f4-map (make-sparse-keymap))
(define-key global-map [(f4)] f4-map)
(define-key f4-map "-" 'dss/clojure-run-tests)
(define-key f4-map "c" 'dss/slime-repl-clear)
(define-key f4-map "p" 'dss/clojure-jump-to-project)
(define-key f4-map "j" 'dss/clojure-jump-between-tests-and-code)

(provide 'zoo-clojure)
