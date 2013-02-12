(require 'zoo-paredit)
(require 'zoo-rainbow-delimiters)
(require 'smex)
(require 'hl-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dss/in-syntax-p (syntax-type)
  "This only answers if you're in a comment or string at the moment."
  (eq syntax-type (syntax-ppss-context (syntax-ppss))))

(defun dss/in-string-p ()
  (dss/in-syntax-p 'string))


(defun dss/in-comment-p ()
  (dss/in-syntax-p 'comment))


(defun dss/blank-line-p ()
  "Return non-nil iff current line is blank."
  (save-excursion
    (beginning-of-line)
    (looking-at "\\s-*$")))

(defun dss/beginning-of-string ()
  "Go to beginning of string around point.
Do nothing if not in string."
  ;; from loveshack's python-beginning-of-string
  (interactive)
  (if (and (not (dss/in-string-p))
           (save-excursion
             (backward-char)
             (dss/in-string-p)))
      (backward-char))
  (let ((state (syntax-ppss)))
    (when (eq 'string (syntax-ppss-context state))
      (goto-char (nth 8 state)))))

(defun dss/flash-region (beg end)
  (interactive "r")
  (let ((ovl (make-overlay beg end))
        (was-mark-active mark-active)
        (hl-line-mode-on hl-line-mode))
    (setq mark-active nil)
    (overlay-put ovl 'face 'highlight)
    (run-with-timer 0.5 nil
                    (lambda (ovl was-mark-active)
                      (delete-overlay ovl)
                      (setq mark-active was-mark-active))
                    ovl was-mark-active)))

(defun dss/indent-sexp ()
  "http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
can be used from any coding major mode"
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point))))
        (dss/flash-region beg end)
        (set-marker end-marker end)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))))))
(defun dss/indent-defun ()
  (interactive)
  (save-excursion
    (dss/out-sexp)
    (forward-char)
    (dss/indent-sexp)))

(defun dss/out-sexp (&optional level forward syntax)
  "Skip out of any nested brackets.
Skip forward if FORWARD is non-nil, else backward.
If SYNTAX is non-nil it is the state returned by `syntax-ppss' at point.
Return non-nil if and only if skipping was done."
  (interactive)
  (if (dss/in-string-p)
      (dss/beginning-of-string))
  (progn
    (let* ((depth (syntax-ppss-depth (or syntax (syntax-ppss))))
           (level (or level depth))
           (forward (if forward -1 1)))
      (unless (zerop depth)
        (if (> depth 0)
            ;; Skip forward out of nested brackets.
            (condition-case () ; beware invalid syntax
                (progn (backward-up-list (* forward level)) t)
              (error nil))
          ;; Invalid syntax (too many closed brackets).
          ;; Skip out of as many as possible.
          (let (done)
            (while (condition-case ()
                       (progn (backward-up-list forward)
                              (setq done t))
                     (error nil)))
            done))))))

(defun dss/eval-defun ()
  "The built-in eval-defun doesn't choose the top level forms I would expect"
  (interactive)
  (dss/indent-defun)
  (save-excursion
    (dss/out-sexp nil t)
    (cond ((or (equal major-mode 'clojure-mode)
               (equal major-mode 'slime-repl-mode))
           (slime-eval-last-expression))
          (t (progn
               (eval-last-sexp nil)
               (smex-update))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(evil-define-key 'normal emacs-lisp-mode-map
  (kbd ",eb") 'eval-buffer
  (kbd ",el") 'eval-last-sexp
  (kbd ",ef") 'dss/eval-defun
  (kbd ",rt") 'ert)

(evil-define-key 'normal lisp-interaction-mode-map
  (kbd ",eb") 'eval-buffer
  (kbd ",el") 'eval-last-sexp
  (kbd ",ef") 'dss/eval-defun
  (kbd ",rt") 'ert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zoo/emacs-lisp-mode-hook ()
  (interactive)
  (zoo/turn-on-paredit)
  (zoo/turn-on-rainbow-delimiters)
  (eldoc-mode 1))

(add-hook 'emacs-lisp-mode-hook 'zoo/emacs-lisp-mode-hook)

(provide 'zoo-elisp)
