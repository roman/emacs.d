(add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get/ghc-mod/elisp"))
(require 'ghc)
(require 'zoo.path)

(defun zoo-haskell/is-cabal-dev-present? ()
  (let ((current-dir (zoo.path/pwd)))
    (and (zoo.path/rfind-file "*.cabal" current-dir)
         (zoo.path/rfind-file "cabal-dev" current-dir))))

(defun zoo-haskell/set-compile-command ()
  (if (zoo-haskell/is-cabal-dev-present?)
      (progn
        (set (make-local-variable 'haskell-program-name)
              "cabal-dev ghci")
        (set (make-local-variable 'default-directory)
             (zoo.path/rfind-dir "*.cabal" (zoo.path/pwd)))
        (set (make-local-variable 'compile-command)
             "cabal-dev build"))))

(setq haskell-mode-hook nil)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook
          (lambda ()
            (ghc-init)
            (zoo-haskell/set-compile-command)))


(provide 'zoo-haskell)
