
;; Flymake setup for checkstyle.
;;
;; This setup of flymake assumes there is a checkstyle binary
;; on your path. Given that running a new instance of the JVM
;; with flymake is uber-slow, we recommend you use a nailgun
;; server to run the checkstyle API.

(require 'zoo-flymake)

(defun flymake-java-checkstyle-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-with-folder-structure
                     ; ^ Don't create a local file with the _flymake postfix
                     ))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "checkstyle -c /usr/share/checkstyle/sun_checks.xml " (list local-file))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.java$"
              flymake-java-checkstyle-init
              flymake-simple-cleanup
              flymake-get-real-file-name)
            flymake-allowed-file-name-masks))

;; Checkstyle error message format is already supported in
;; flymake-err-line-patterns so we don't need to add a new
;; entry there

(provide 'zoo-java)
