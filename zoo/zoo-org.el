(require 'org)
(require 'org-clock)
(require 'org-remember)
(require 'org-agenda)
(require 'org-timer)
(require 'zoo-keybinding)

;; Most of the settings are being ported from Tavis Rudd's org mode
;; configuration
;; https://github.com/tavisrudd/emacs.d

;; org persist file
(setq org-clock-persist-file
      (concat zoo-ephemeral-dir ".org-clock-save.el"))

(setq org-hide-leading-stars t)
(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)

;; ctrl-a/e will respect org-mode entries
;; jump to the start of the headline
(setq org-special-ctrl-a/e t)
;; respect tags when killing line on a heading
(setq org-special-ctrl-k t)
(setq org-return-follows-link t)

;; Save the clock and entry when I close emacs
(setq org-clock-persist t)

;; Check a clock that was left behind open when
;; starting emacs
(org-clock-persistence-insinuate)

;; Store at least 35 clocks in memory
(setq org-clock-history-length 35)

;; Don't ask me to resume the clock during load
(setq org-clock-persist-query-resume nil)

;; Resume clock on task that has an open clock
(setq org-clock-in-resume t)

;; When clocking in, change the status of the item to
;; STARTED
(setq org-clock-in-switch-to-state "STARTED")

;; Have a special :CLOCK: drawer for clocks
(setq org-clock-into-drawer "CLOCK")

;; Don't register clocks with zero-time length
(setq org-clock-out-remove-zero-time-clocks t)

;; Stop clock when a task gets to state DONE.
(setq org-clock-out-when-done t)

;; Resolve open-clocks if iddle more than 30 minutes
(setq org-clock-idle-time 30)

;; Activate single letter commands at the beginning of
;; a headline
(setq org-use-speed-commands t)

;; when changing the item to DONE, Don't add anything
(setq org-log-done nil)

;; Add all notes and timestamps to the LOGBOOK drawer
(setq org-log-into-drawer "LOGBOOK")

;; When task is refilled, rescheduled or redeadline add
;; a timestamp to the task
(setq org-log-refile 'time)
(setq org-log-reschedule 'time)
(setq org-log-redeadline 'time)

(setq org-log-note-headings
      '((done .  "CLOSING NOTE %t")
        (state . "State %-12s from %-12S %t")
        (note .  "Note taken on %t")
        (reschedule .  "Rescheduled from %S on %t")
        (delschedule .  "Not scheduled, was %S on %t")
        (redeadline .  "New deadline from %S on %t")
        (deldeadline .  "Removed deadline, was %S on %t")
        (refile . "Refiled from %s to %S on %t")
        (clock-out . "")))


;; Avoid adding a blank line after doing alt-return on an entry.
(setq org-blank-before-new-entry '((heading . auto)
                                   (plain-list-item . auto)))

;; When hitting alt-return on a header, please create a new one without
;; messing up the one I'm standing on.
(setq org-insert-heading-respect-content t)

(setq org-log-done nil)

(setq org-log-into-drawer "LOGBOOK")

(setq org-log-refile 'time)
(setq org-log-reschedule 'time)
(setq org-log-redeadline 'time)

;; Show me pretty colors on babel source code inside org.
(setq org-src-fontify-natively t)

;; Avoid setting entries as DONE when there are still sub-entries
;; that are not DONE.
(setq org-enforce-todo-dependencies t)

;; Allow to iterate easily between todo-keywords using meta->/meta-<
(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; States that a todo can have
(setq org-todo-keywords
  '((sequence "TODO(t)" "TODAY(y!)" "|" "STARTED(s!)" "|" "PAUSED(p!)" "|" "DONE(d!/!)")
    (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "OPEN(O@)" "|" "CANCELLED(c@/!)")))


(defun zoo/org-clocking-p ()
  (interactive)
  (and (fboundp 'org-clocking-p)
       (org-clocking-p)))


(defun dss/org-current-timestamp ()
  (let ((fmt (concat
              "[" (substring (cdr org-time-stamp-formats) 1 -1) "]")))
    (format-time-string fmt)))

(defun dss/org-current-clock-id ()
  "Get the id of the current item being clocked."
  (save-window-excursion
    (save-excursion
      (org-clock-goto)
      (org-id-get-create))))

(defun dss/org-insert-heading-hook ()
  (interactive)
  ;; Create an ID for the current item
  (org-id-get-create)
  (org-set-property "ADDED" (dss/org-current-timestamp))
  (if (zoo/org-clocking-p)
      ;; ^ If a clock is active, add a reference to the task
      ;; that is clocked in
      (org-set-property "CLOCK-WHEN-ADDED" (dss/org-current-clock-id))))
(add-hook 'org-insert-heading-hook 'dss/org-insert-heading-hook)

;; Ask for an estimated time of completeness when clocking
;; in for the first time
(defun dss/org-mode-ask-effort ()
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))
(add-hook 'org-clock-in-prepare-hook 'dss/org-mode-ask-effort)

(setq org-capture-templates
      '(("s" "sub-heading to clocked-in heading" entry (clock) "* TODO %?\n %i\n %a")
        ("k" "ckechbox to clocked-in heading" checkitem (clock) "- [ ] %A")))

;; When evaluating code in org, don't ask me!
(defun dss/babel-no-confirm ()
  (interactive)
  (setq org-confirm-babel-evaluate nil))

;; Possible tags to choose
(setq org-tag-alist
      '((:startgroup . nil)
        ("today" . ?t)
        ("tomorrow" . ?m)
        ("next" . ?n)
        ("learning" . ?l)
        ("tweaking" . ?t)
        (:endgroup . nil)))

;; Pretty styling for the different keywords of a TODO item
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("TODAY" :foreground "color-27" :weight bold)
        ("STARTED" :foreground "color-27" :weight bold)
        ("PAUSED" :foreground "gold" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("SOMEDAY" :foreground "magenta" :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)))

;; BABEL supported languages
(setq org-babel-load-languages
      '((clojure . t)
        (emacs-lisp . t)))

(defun dss/org-clock-in-select ()
  "C-u C-c C-x C-i -> org-clock-select-task"
  (interactive)
  (org-clock-in '(4)))

(defun dss/org-clock-goto-select-task ()
  "C-u C-c C-x C-i -> org-clock-goto via org-clock-select-task"
  (interactive)
  (org-clock-goto '(4)))

;; The f8 keybinding is going to be dedicated for org mode
;; use only, that's why we define the keybindings for f8 in this
;; module
(define-key f8-map "i" 'org-clock-in)
(define-key f8-map "o" 'org-clock-out)
(define-key f8-map "r" 'org-capture)
(define-key f8-map "c" 'org-clock-cancel)
(define-key f8-map "-" 'org-clock-goto)
(define-key f8-map "_" 'dss/org-clock-in-select)
(define-key f8-map "'" 'dss/org-clock-goto-select-task)
(define-key f8-map "t" 'org-set-tags-command)
(define-key f8-map "." 'org-store-link)
(define-key f8-map "/" 'org-tags-view)

;;;;;;;;;;;;;;;;;;;;
;; Future release of evil-org

(define-minor-mode evil-org-mode
  "Minor mode for setting up Evil with org mode in a single buffer"
  :keymap '())

(defun evil-org-insert-heading ()
  (interactive)
  (end-of-line)
  (cond
   ((org-at-item-checkbox-p) (org-insert-item 'checkbox))
   (t (org-insert-heading)))
  (evil-insert-state))

(defun evil-org-insert-subheading (arg)
  (interactive "P")
  (end-of-line)
  (cond
   ((org-at-item-checkbox-p)
    (progn
      (org-insert-item 'checkbox)
      (org-indent-item)))
   (t (org-insert-subheading arg)))
  (evil-insert-state))

(evil-define-key 'normal evil-org-mode-map
  (kbd ")") 'outline-next-visible-heading
  (kbd "(") 'outline-previous-visible-heading
  (kbd "TAB") 'org-cycle
  (kbd "gp") 'outline-up-heading
  (kbd "M-o") 'zoo/org-insert-heading
  (kbd "C-o") 'zoo/org-insert-subheading)

;;;;;;;;;;;;;;;;;;;;

(evil-org-mode 1)

(provide 'zoo-org)
