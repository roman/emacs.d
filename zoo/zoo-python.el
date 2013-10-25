;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq pycodechecker "pylint_etc_wrapper.py")

;; (when (load "flymake" t)
;;   (load-library "flymake-cursor")
;;   (defun dss/flymake-pycodecheck-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list pycodechecker (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" dss/flymake-pycodecheck-init)))

;; And here are two little helpers for quickly silencing a warning message:

(defun dss/pylint-msgid-at-point ()
  (interactive)
  (let (msgid
        (line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info msgid)
      (if (eq (car elem) line-no)
            (let ((err (car (second elem))))
              (setq msgid (second (split-string (flymake-ler-text err)))))))))

(defun dss/pylint-silence (msgid)
  "Add a special pylint comment to silence a particular warning."
  (interactive
   (list (read-from-minibuffer "msgid: " (dss/pylint-msgid-at-point))))
  (save-excursion
    (comment-dwim nil)
    (if (looking-at "pylint:")
        (progn (end-of-line)
               (insert ","))
        (insert "pylint: disable-msg="))
    (insert msgid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq virtual-env (getenv "VIRTUAL_ENV"))
(defvar dss-ropemacs-loaded nil)
(defun dss/ropemacs-init ()
  (interactive)
  (unless dss-ropemacs-loaded
    (if (not (equal virtual-env nil))
        (setq load-path (append
                         (list (concat virtual-env "/src/pymacs" ))
                         load-path)))
    (require 'pymacs)
    (if (not (boundp 'ropemacs-global-prefix))
        (setq ropemacs-global-prefix nil))
    (pymacs-load "ropemacs" "rope-")
    (setq ropemacs-enable-autoimport nil)
    (define-key ropemacs-local-keymap (kbd "M-/") nil)
    (setq dss-ropemacs-loaded t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dss/python-mode-hook ()
  (interactive)
  ;; (which-function-mode t)
  (setq mode-name "PY:")
  (setq py-python-command-args '("-colors" "Linux"))
  (if (and (string-match "\\.py$" (buffer-name))
           ;; and isn't a py-shell tmp buffer:
           (not (string-match "python-" (buffer-name))))
      (progn
        (flymake-mode t)
        ;; (dss/ropemacs-init)
        ;; (ropemacs-mode t)
        )))

(add-hook 'python-mode-hook 'dss/python-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'zoo-python)
