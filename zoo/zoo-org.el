(require 'zoo-keybinding)
;; Most of the settings are being ported from Tavis Rudd's org mode
;; configuration
;; https://github.com/tavisrudd/emacs.d

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

(setq org-use-speed-commands t)

(setq org-log-done nil)
(setq org-log-into-drawer "LOGBOOK")
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
(setq org-blank-before-new-entry '((heading) (plain-list-item)))

;; When hitting alt-return on a header, please create a new one without
;; messing up the one I'm standing on.
(setq org-insert-heading-respect-content t)

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

;; The f8 keybinding is going to be dedicated for org mode
;; use only, that's why we define the keybindings for f8 in this
;; module

(defun dss/org-clock-in-select ()
  "C-u C-c C-x C-i -> org-clock-select-task"
  (interactive)
  (org-clock-in '(4)))

(defun dss/org-clock-goto-select-task ()
  "C-u C-c C-x C-i -> org-clock-goto via org-clock-select-task"
  (interactive)
  (org-clock-goto '(4)))

(define-key f8-map "i" 'org-clock-in)
(define-key f8-map "o" 'org-clock-out)
(define-key f8-map "r" 'org-capture)
(define-key f8-map "c" 'org-clock-cancel)
(define-key f8-map "-" 'org-clock-goto)
(define-key f8-map "_" 'dss/org-clock-in-select)
(define-key f8-map "'" 'dss/org-clock-goto-select-task)
(define-key f8-map "t" 'org-set-tags-command)
(define-key f8-map "/" 'org-tags-view)
(define-key f8-map "." 'org-todo)


(provide 'zoo-org)
