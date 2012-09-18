;; Flymake setup for checkstyle.
;;
;; This setup of flymake assumes there is a checkstyle binary
;; on your path. Given that running a new instance of the JVM
;; with flymake is uber-slow, we recommend you use a nailgun
;; server to run the checkstyle API.

(require 'zoo-flymake)

(defvar zoo-checkstyle-program "checkstyle-drip")

(defun zoo/flymake-java-checkstyle-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-with-folder-structure))
                     ; ^ Don't create a local file with the _flymake postfix 
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    

    (list zoo-checkstyle-program (list local-file))
    ;; (list "checkstyle" (list "-c"
    ;;                          "/usr/share/checkstyle/sun_checks.xml"
    ;;                          local-file))
    ))

(defvar zoo/flymake-java-checkstyle-err-pattern-no-column
  '("^\\(.+\\):\\([0-9]+\\): \\(.+\\)$" 1 2 nil 3))

(defvar zoo/flymake-java-checkstyle-err-pattern-with-column
  '("^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): \\(.+\\)$" 1 2 3 4))

(defun zoo/flymake-java-checkstyle-load ()
  (interactive)

  ;; Setting the current buffer to execute the java-checkstyle prg
  (set (make-local-variable 'flymake-allowed-file-name-masks)
       '(("." zoo/flymake-java-checkstyle-init
              flymake-simple-cleanup
              flymake-get-real-file-name)))

  ;; Setting error patterns regexps
  (set (make-local-variable 'flymake-err-line-patterns)
       (list zoo/flymake-java-checkstyle-err-pattern-no-column
             zoo/flymake-java-checkstyle-err-pattern-with-column))

  ;; Only allowing flymake mode when the binary we need is there
  (if (executable-find zoo-checkstyle-program)
      (progn
        (flymake-mode t))
    (message (format "Not enabling flymake: %s command not found"
                     zoo-checkstyle-program))))

;; Add all the setup of flymake in a hook
(defun zoo/java-mode-hook ()
  (interactive)
  (zoo/flymake-java-checkstyle-load)
  (defadvice flymake-post-syntax-check
    (before zoo-flymake-force-check-was-interrupted activate)
    (setq flymake-check-was-interrupted t)))

(add-hook 'java-mode-hook 'zoo/java-mode-hook)

(provide 'zoo-java)
