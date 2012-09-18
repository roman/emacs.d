(require 'evil nil)
(require 'surround nil)
(require 'paredit nil)

(defun zoo/turn-on-paredit ()
  (interactive)
  (paredit-mode 1))

(evil-define-operator zoo/evil-paredit-delete (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE respecting parenthesis.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (evil-yank beg end type register yank-handler)
  (if (eq type 'block)
      (evil-apply-on-block #'delete-region beg end)
    (paredit-kill-region beg end))
  ;; place cursor on beginning of line
  (when (and (evil-called-interactively-p)
             (eq type 'line))
    (evil-first-non-blank)))

(evil-define-operator zoo/evil-paredit-delete-line (beg end type register yank-handler)
  "Delete to end of line respecting parenthesis."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  ;; act linewise in Visual state
  (let* ((beg (or beg (point)))
         (end (or end beg)))
    (when (evil-visual-state-p)
      (unless (memq type '(line block))
        (let ((range (evil-expand beg end 'line)))
          (setq beg (evil-range-beginning range)
                end (evil-range-end range)
                type (evil-type range))))
      (evil-exit-visual-state))
    (cond
     ((eq type 'block)
      ;; equivalent to $d, i.e., we use the block-to-eol selection and
      ;; call `evil-delete'. In this case we fake the call to
      ;; `evil-end-of-line' by setting `temporary-goal-column' and
      ;; `last-command' appropriately as `evil-end-of-line' would do.
      (let ((temporary-goal-column most-positive-fixnum)
            (last-command 'next-line))
        (zoo/evil-paredit-delete beg end 'block register yank-handler)))
     ((eq type 'line)
      (zoo/evil-paredit-delete beg end type register yank-handler))
     (t
      (zoo/evil-paredit-delete beg (line-end-position) type register yank-handler)))))


(evil-define-operator zoo/evil-paredit-change
  (beg end type register yank-handler delete-func)
  "Change text from BEG to END with TYPE respecting parenthesis.
Save in REGISTER or the kill-ring with YANK-HANDLER.
DELETE-FUNC is a function for deleting text, default `evil-delete'.
If TYPE is `line', insertion starts on an empty line.
If TYPE is `block', the inserted text in inserted at each line
of the block."
  (interactive "<R><x><y>")
  (let ((delete-func (or delete-func #'zoo/evil-paredit-delete))
        (nlines (1+ (- (line-number-at-pos end)
                       (line-number-at-pos beg)))))
    (funcall delete-func beg end type register yank-handler)
    (cond
     ((eq type 'line)
      (evil-open-above 1))
     ((eq type 'block)
      (evil-insert 1 nlines))
     (t
      (evil-insert 1)))))

(evil-define-operator zoo/evil-paredit-change-line (beg end type register yank-handler)
  "Change to end of line respecting parenthesis."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (zoo/evil-paredit-change beg end type register yank-handler))

(evil-define-key 'normal paredit-mode-map
  (kbd ",>")  'paredit-forward-slurp-sexp
  (kbd ",<")  'paredit-backward-slurp-sexp
  (kbd ";>")  'paredit-backward-barf-sexp
  (kbd ";<")  'paredit-forward-barf-sexp
  (kbd ",s")  'paredit-split-sexp
  (kbd ",j")  'paredit-join-sexps
  (kbd ",ks") 'paredit-splice-sexp
  (kbd ",kf") 'paredit-splice-sexp-killing-forward
  (kbd ",kb") 'paredit-splice-sexp-killing-backward
  (kbd ",ku") 'paredit-raise-sexp
  (kbd "(")   'paredit-backward
  (kbd ")")   'paredit-forward
  (kbd "d")   'zoo/evil-paredit-delete
  (kbd "c")   'zoo/evil-paredit-change
  (kbd "C")   'zoo/evil-paredit-change-line
  (kbd "D")   'zoo/evil-paredit-delete-line
  (kbd "x")   'paredit-forward-delete)

(provide 'zoo-paredit)
