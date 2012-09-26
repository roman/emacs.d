(require 'paredit)
(require 'zoo-paredit)
(require 'zoo-rainbow-delimiters)
(require 'zoo-keybinding)

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
    (end-of-buffer)))

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

(defun zoo/jump-to-last-error-on-clj-repl ()
  (interactive)
  (if (and (boundp 'clojure-test-error-count)
             (> clojure-test-error-count 0))
    (with-current-buffer "*slime-repl clojure*"
      (end-of-buffer)
      (let ((case-fold-search nil))
        (search-backward "ERROR")))
    (message "No clojure test had error.")))

(defun zoo/jump-to-last-failure-on-clj-repl ()
  (interactive)
  (if (and (boundp 'clojure-test-error-count)
             (> clojure-test-failure-count 0))
    (with-current-buffer "*slime-repl clojure*"
      (end-of-buffer)
      (let ((case-fold-search nil))
        (search-backward "FAIL")))
    (message "No clojure test failed.")))

(defvar zoo/slime-compile-stack nil)
(defvar zoo/slime-last-compiled nil)

(defun zoo/slime-compile-hook (notes)
  (if notes
      (progn
        (setq zoo/slime-compile-stack nil)
        (run-with-timer 0.5 nil
         (lambda () (ignore-errors
          (sit-for 0.5)
          (pop-to-buffer "*SLIME Compilation*")
          (progn
            (beginning-of-buffer)
            (call-interactively 'compilation-next-error)
            (call-interactively 'compile-goto-error)))))

        ;; (when zoo/slime-last-compiled
        ;;   (pop-to-buffer zoo/slime-last-compiled))
                                        ;(message notes)
        )
    (when zoo/slime-compile-stack
      (let ((next (pop zoo/slime-compile-stack)))
        (cond
         ((functionp next) (funcall next))
         ((stringp next)
          (with-current-buffer next
            (setq zoo/slime-last-compiled next)
            (slime-compile-and-load-file))))))))

(add-hook 'slime-compilation-finished-hook 'zoo/slime-compile-hook)

(setq zoo/clj-working-buffers
      '("keys.clj" "util.clj" "domain.clj" "reify.clj"
        "tickets.clj" "orders_with_credit_allowance.clj"
        "domain_test.clj"))

(defun zoo/add-clj-save-hooks ()
  (interactive)
  (dolist (b-name zoo/clj-working-buffers)
    (when (get-buffer b-name)
      (with-current-buffer b-name
          (dss/add-after-save-hook
           'zoo/compile-and-test-clj)))))

(defun zoo/compile-and-test-clj ()
  (interactive)
  (setq zoo/slime-compile-stack
        (append (cdr zoo/clj-working-buffers)
                (list #'(lambda ()
                     (with-current-buffer "domain_test.clj"
                       (clojure-test-run-tests))))))
  (with-current-buffer (car zoo/clj-working-buffers)
    (slime-compile-and-load-file)))

(defun zoo/set-clojure-keybindings ()
  (interactive)
  (local-set-key (kbd "<f4> -") 'dss/clojure-run-tests)
  (local-set-key (kbd "<f4> e") 'slime-eval-buffer)
  (local-set-key (kbd "<f4> c") 'dss/slime-repl-clear)
  (local-set-key (kbd "<f4> p") 'dss/clojure-jump-to-project)
  (local-set-key (kbd "<f4> j") 'dss/clojure-jump-between-tests-and-code)
  (evil-define-key 'normal clojure-mode-map (kbd "gle") 'zoo/jump-to-last-error-on-clj-repl)
  (evil-define-key 'normal clojure-mode-map (kbd "glf") 'zoo/jump-to-last-failure-on-clj-repl))

(global-set-key (kbd "<f4> m") 'mark-sexp)
(global-set-key (kbd "<f4> ;") 'goto-last-change)
(global-set-key (kbd "<f4> /") 'evil-jump-item)

(defun zoo/clojure-mode-hook ()
  (interactive)
  (setq clojure-test-ns-segment-position 1)
  (zoo/turn-on-paredit)
  (zoo/turn-on-rainbow-delimiters)
  (zoo/set-clojure-keybindings))

(add-hook 'clojure-mode-hook 'zoo/clojure-mode-hook)

;; (define-clojure-indent
;;   (entity 'defun)
;;   (events 'defun)
;;   (defdomain 'defun)
;;   (create-'domain defun)
;;   (properties 'defun))

(provide 'zoo-clojure)
