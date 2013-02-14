(defun find/ido-search-file (dir pattern exclude-pattern)
  (let* ((file-list
          (split-string
           (shell-command-to-string
            (concat
             "DIR=" dir ";"
             "find $DIR -type f -regex '" pattern "'"
             " -not -regex '" exclude-pattern "' "
             "| sed -e's#'$DIR'##'"))))
         (choice (ido-completing-read "Which file: " file-list)))
    (find-file (concat dir choice))))


(defun zoo/find-zoo-elisp ()
  (interactive)
  (find/ido-search-file "~/.emacs.d/zoo/"
                              ".*\\.el"
                              ".*\\.elc"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zoo/occur-this ()
  (interactive)
  (occur (thing-at-point 'symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(evil-set-initial-state 'occur-mode 'motion)
(evil-set-initial-state 'grep-mode 'motion)

(evil-define-key 'motion occur-mode-map
  (kbd "RET") 'occur-mode-goto-occurrence)

(evil-define-key 'normal global-map
  (kbd ",f0") 'zoo/find-zoo-elisp
  (kbd "<f4>o") 'zoo/occur-this)

(provide 'zoo-find)
