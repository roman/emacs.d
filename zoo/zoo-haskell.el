;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get/ghc-mod/elisp"))
(require 'evil)
(require 'proctor-mode)
(require 'zoo-flymake)
(require 'zoo.path)

;;;;;;;;;;;;;;;;;;;;
;; Flymake with ghc-mod
;;;;;;;;;;;;;;;;;;;;
;;
;; NOTE:
;;
;; For some reason, ghc-mod is not working as expected so I'm
;; re-implementing most of it
(defvar zoo/ghc-mod-subcmd "check")
(defvar zoo/flymake-haskell-warning-pattern
  '("^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): Warning: \\(.+\\)$" 1 2 3 4))
(defvar zoo/flymake-haskell-err-pattern
  '("^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)$" 1 2 3 4))

(defun zoo/flymake-haskell-toggle-command ()
  (interactive)
  (cond
   ((string-equal zoo/ghc-mod-subcmd "check")
    (progn
      (message "Flymake with ghc-mod lint")
      (setq zoo/ghc-mod-subcmd "lint")))
   ((string-equal zoo/ghc-mod-subcmd "lint")
    (progn
      (message "Flymake with ghc-mod check")
      (setq zoo/ghc-mod-subcmd "check")))))

(defun zoo/flymake-haskell-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-with-folder-structure))
         (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
    (list "ghc-mod" (list zoo/ghc-mod-subcmd local-file))))

(defun zoo/flymake-haskell-load ()
  (interactive)
  (set (make-local-variable 'flymake-log-level) 0)
  (set (make-local-variable 'flymake-allowed-file-name-masks)
       '(("." zoo/flymake-haskell-init
              flymake-simple-cleanup
              flymake-get-real-file-name)))
  (set (make-local-variable 'flymake-err-line-patterns)
       (list zoo/flymake-haskell-warning-pattern
             zoo/flymake-haskell-err-pattern))

  (if (executable-find "ghc-mod")
      (progn
        (flymake-mode t))
    (message (format "Not enabling flymake: `ghc-mod` command not found"))))


;;;;;;;;;;;;;;;;;;;;
;; Building Utilities
;;;;;;;;;;;;;;;;;;;;
;;
;; NOTE:
;; Using `zoo.path/rfind-file` instead of `locate-dominating-file`
;; because the latter doesn't accept a regexp as the file name
(defun zoo/is-cabal-dev-present? ()
  (let* ((current-dir (zoo.path/pwd))
         (cabal-dev-folder (zoo.path/rfind-dir "cabal-dev" current-dir))
         (cabal-file-folder (zoo.path/rfind-dir "*.cabal" current-dir)))
    (and (not (null cabal-dev-folder))
         (not (null cabal-file-folder))
         (string= cabal-dev-folder cabal-file-folder))))

(defun zoo/haskell-set-compile-command ()
  (if (zoo/is-cabal-dev-present?)
      (progn
        (set (make-local-variable 'haskell-program-name)
              "cabal-dev ghci")
        (set (make-local-variable 'default-directory)
             (zoo.path/rfind-dir "*.cabal" (zoo.path/pwd)))
        (set (make-local-variable 'compile-command)
             "cabal-dev build"))))

(defun zoo/haskell-compile ()
  (interactive)
  (compile compile-command))

(defun zoo/set-haskell-ghci-program
  (if (zoo/is-cabal-dev-present?)
      (if (and (executable-find "screen")
               (executable-find "cabal-dev-ghci"))
          (progn
            (setq haskell-ghci-program-name "screen")
            (setq haskell-ghci-program-args (list "-s" "cabal-dev-ghci")))
        (progn
          (setq haskell-ghci-program-name "cabal-dev")
          (setq haskell-ghci-program-args (list "ghci"))))))

(defun zoo/haskell-add-extra-keybindings ()
  (interactive)
  (evil-define-key 'normal haskell-mode-map
    (kbd ",b") 'zoo/haskell-compile))

(defun zoo/haskell-mode-hook ()
  (interactive)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-simple-indent)
  (zoo/haskell-set-compile-command)
  (zoo/flymake-haskell-load)

  (define-key haskell-mode-map
    (kbd "C-x C-s")
    'haskell-mode-save-buffer))

(add-hook 'haskell-mode-hook 'zoo/haskell-mode-hook)

(setq haskell-stylish-on-save t)

(evil-define-key 'normal haskell-mode-map
    (kbd ",b") 'zoo/haskell-compile)

(provide 'zoo-haskell)
