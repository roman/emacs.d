(add-to-list 'load-path (expand-file-name "~/.emacs.d/zoo"))
(require 'zoo-dependencies)
(require 'zoo-rainbow-delimiters)
(require 'zoo-paredit)
(require 'zoo-basics)
(require 'zoo-ido)
(require 'zoo-term)
(require 'zoo-evil)
(require 'zoo-theme)
(require 'zoo-whitespace)
(require 'zoo-ruby)
(require 'zoo-clojure)
(require 'zoo-haskell)

;; clojure

(defun dss/slime-repl-clear ()
  (interactive)
  (save-window-excursion
    (slime-switch-to-output-buffer)
    (slime-repl-clear-buffer)
    (end-of-buffer)
    (dss/sync-point-all-windows)))

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

(defun dss/clojure-run-tests ()
  (interactive)
  (save-window-excursion
    (if (not (dss/clojure-in-tests))
        (clojure-jump-to-test))
    (clojure-test-run-tests)))

(defun dss/clojure-load-current-file ()
  (interactive)
  (save-buffer)
  (if (not (string-match-p
            "project\\.clj"
            (buffer-file-name)))
      (slime-compile-and-load-file)))

(defvar f4-map (make-sparse-keymap))
(define-key global-map [(f4)] f4-map)
(define-key f4-map "-" 'dss/clojure-run-tests)
(define-key f4-map "c" 'dss/slime-repl-clear)
(define-key f4-map "p" 'dss/clojure-jump-to-project)
(define-key f4-map "j" 'dss/clojure-jump-between-tests-and-code)

;; hooks


(defun dss/-get-hook-funcs (hook)
  (delq nil (mapcar
             (lambda (e) (if (symbolp e) e))
             hook)))

(defun dss/-get-hook-funcs-names (hook)
  (mapcar 'symbol-name
          (dss/-get-hook-funcs
           (if (symbolp hook)
               (symbol-value hook)
             hook))))

(defun dss/-get-all-hooks ()
  (let (hlist (list))
    (mapatoms (lambda (a)
                (if (and (not (null (string-match ".*-hook"
                                                  (symbol-name a))))
                         (not (functionp a)))
                    (add-to-list 'hlist a))))
    hlist))

;;; mapatoms (symbolp x) (functionp x) (commandp (symbol-function x)) (fbound x)
;;; also see http://stackoverflow.com/questions/605785/how-do-i-get-a-list-of-emacs-lisp-non-interactive-functions

(defun dss/remove-from-hook (hook fname &optional local)
  (interactive
   (let ((hook (intern (ido-completing-read
                        "Which hook? "
                        (mapcar #'symbol-name (dss/-get-all-hooks))))))
     (list hook
           (ido-completing-read "Which? " (dss/-get-hook-funcs-names hook)))))
  (remove-hook hook
               (if (stringp fname)
                   (intern fname)
                 fname)
               local))

(defun dss/remove-after-save-hook (fname)
  (interactive (list (ido-completing-read
                      "Which? "
                      (dss/-get-hook-funcs-names after-save-hook))))
  (dss/remove-from-hook 'after-save-hook fname t))

(defun dss/add-after-save-hook (fname)
  (interactive "aWhich function: ")
  (add-hook 'after-save-hook fname t t))
