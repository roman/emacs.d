(add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get/ghc-mod/elisp"))
(require 'zoo-flymake)
(require 'zoo.path)
(require 'ghc)

;; NOTE:
;; Using `zoo.path/rfind-file` instead of `locate-dominating-file`
;; because the latter doesn't accept a regexp as the file name

(defun zoo/is-cabal-dev-present? ()
  (let ((current-dir (zoo.path/pwd)))
    (and (zoo.path/rfind-file "*.cabal" current-dir)
         (zoo.path/rfind-file "cabal-dev" current-dir))))

(defun zoo/haskell-set-compile-command ()
  (if (zoo-haskell/is-cabal-dev-present?)
      (progn
        (set (make-local-variable 'haskell-program-name)
              "cabal-dev ghci")
        (set (make-local-variable 'default-directory)
             (zoo.path/rfind-dir "*.cabal" (zoo.path/pwd)))
        (set (make-local-variable 'compile-command)
             "cabal-dev build"))))

;; Reset the haskell-mode-hook
(setq haskell-mode-hook nil)

(defun zoo/haskell-mode-hook ()
  (interactive)
  (ghc-init)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-simple-indent)
  (zoo/haskell-set-compile-command))

(add-hook 'haskell-mode-hook 'zoo/haskell-mode-hook)

(provide 'zoo-haskell)
