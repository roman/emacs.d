;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get/ghc-mod/elisp"))
(require 'evil)
(require 'proctor-mode)
(require 'zoo-flymake)
(require 'zoo.path)


(setq haskell-stylish-on-save t)

;;;;;;;;;;;;;;;;;;;;
;; Flymake with ghc-mod
;;;;;;;;;;;;;;;;;;;;
(setq haskell-stylish-on-save t)
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

(defun zoo/haskell-add-type-decl ()
  (interactive)
  (save-excursion
    (condition-case nil
        (progn
          (beginning-of-line)
          (let ((typ (inferior-haskell-get-result
                      (concat ":type " (haskell-ident-at-point)))))
            (insert typ)
            (insert "\n")))
      (error
       (message "error inserting type")))))

(defun zoo/haskell-mode-hook ()
  (interactive)

  (turn-on-haskell-doc-mode)
  (turn-on-haskell-simple-indent)
  (zoo/haskell-set-compile-command)
  (zoo/flymake-haskell-load))

(add-hook 'haskell-mode-hook 'zoo/haskell-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings

;; (define-key haskell-mode-map
;;     (kbd "C-x C-s")
;;     'save-buffer)

(defun zoo/switch-to-haskell ()
  (interactive)
  (let ((buffer (get-buffer "*haskell*")))
    (when (and buffer
             (y-or-n-p "Do you want to reload ghci? "))
      (process-kill-without-query (get-buffer-process buffer))
      (kill-buffer buffer)))
  (switch-to-haskell))

(evil-define-key 'normal haskell-mode-map
  (kbd ",b")  'zoo/haskell-compile
  (kbd ",at") 'zoo/haskell-add-type-decl
  (kbd ",gi") 'zoo/switch-to-haskell
  (kbd ",fl") 'inferior-haskell-load-file
  (kbd ",fr") 'inferior-haskell-reload-file
  (kbd ",ef") 'inferior-haskell-send-decl
  (kbd ",.")  'inferior-haskell-find-definition
  (kbd ",ii") 'inferior-haskell-info
  (kbd ",it") 'inferior-haskell-type)

(provide 'zoo-haskell)
