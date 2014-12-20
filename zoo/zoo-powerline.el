;;; package --- Summary
;;; Commentary:
(require 'powerline)
(require 'zoo-org)

;;; Code:

(set-face-foreground 'flycheck-fringe-error   "white")
(set-face-background 'flycheck-fringe-error   "#F2777A")

(set-face-foreground 'flycheck-fringe-warning "black")
(set-face-background 'flycheck-fringe-warning "#FFCC66")

(set-face-foreground 'flycheck-fringe-info    "white")
(set-face-background 'flycheck-fringe-info    "#6699CC")

(set-face-attribute 'mode-line nil :height 80)

(defface zoo/powerline-evil-tag-normal-face
  '((t (:foreground "#005F00" :background "#AFD700" :inherit mode-line)))
  "Powerline face for insert mode."
  :group 'powerline)

(defface zoo/powerline-evil-active-mode-line-normal-face
 '((t (:foreground "black" :background "#E6E5E4" :inherit mode-line)))
  "Powerline face for insert mode."
  :group 'powerline)

(defface zoo/powerline-evil-tag-insert-face
  '((t (:foreground "white" :background "#005F87" :inherit mode-line)))
  "Powerline face for insert mode."
  :group 'powerline)

(defface zoo/powerline-evil-active-mode-line-insert-face
 '((t (:foreground "white" :background "#0087AF" :inherit mode-line)))
  "Powerline face for insert mode."
  :group 'powerline)

(defface zoo/powerline-evil-tag-visual-face
  '((t (:foreground "#870000" :background "#FF8700" :inherit mode-line)))
  "Powerline face for insert mode."
  :group 'powerline)

(defface zoo/powerline-evil-tag-emacs-face
  '((t (:foreground "white" :background "#5F005F" :inherit mode-line)))
  "Powerline face for insert mode."
  :group 'powerline)

(defface zoo/powerline-evil-tag-replace-face
  '((t (:foreground "white" :background "#D70000" :inherit mode-line)))
  "Powerline face for insert mode."
  :group 'powerline)

(defface zoo/powerline-evil-tag-motion-face
  '((t (:foreground "white" :background "#FF0080" :inherit mode-line)))
  "Powerline face for insert mode."
  :group 'powerline)

(defface zoo/powerline-evil-tag-operator-face
  '((t (:foreground "white" :background "#363636" :inherit mode-line)))
  "Powerline face for insert mode."
  :group 'powerline)

(defface zoo/powerline-clock-in-face
  '((t (:foreground "white" :background "#F2686C" :inherit mode-line)))
  "Powerline face for insert mode."
  :group 'powerline)


(defun zoo/powerline-evil-tag ()
  "."
  (cond
   ((string= evil-state 'normal)   "N ")
   ((string= evil-state 'insert)   "I ")
   ((string= evil-state 'emacs)    "E ")
   ((string= evil-state 'motion)   "M ")
   ((string= evil-state 'visual)   "V ")
   ((string= evil-state 'operator) "O ")
   ((string= evil-state 'replace)  "R ")))

(defun zoo/powerline-evil-tag-face ()
  "."
  (cond
   ((string= evil-state 'normal)   'zoo/powerline-evil-tag-normal-face)
   ((string= evil-state 'insert)   'zoo/powerline-evil-tag-insert-face)
   ((string= evil-state 'emacs)    'zoo/powerline-evil-tag-emacs-face)
   ((string= evil-state 'motion)   'zoo/powerline-evil-tag-motion-face)
   ((string= evil-state 'operator) 'zoo/powerline-evil-tag-operator-face)
   ((string= evil-state 'visual)   'zoo/powerline-evil-tag-visual-face)
   ((string= evil-state 'replace)  'zoo/powerline-evil-tag-replace-face)))

(defun zoo/powerline-flycheck-status-text ()
  "."
  (cond ((flycheck-has-current-errors-p 'error)
         "✗ ")
        ((flycheck-has-current-errors-p 'warning)
         "⚠ ")
        ((flycheck-has-current-errors-p 'info)
         "☆ ")))

(defun zoo/powerline-flycheck-status-face ()
  "."
  (cond ((flycheck-has-current-errors-p 'error)
         'flycheck-fringe-error)
        ((flycheck-has-current-errors-p 'warning)
         'flycheck-fringe-warning)
        ((flycheck-has-current-errors-p 'info)
         'flycheck-fringe-info)))

(defun zoo/powerline-clock-in  ()
  "."
  (when (not (zoo/org-clocking-p))
    "CLOCK-IN "))

(defun zoo/powerline-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw (zoo/powerline-evil-tag)
                                                    (zoo/powerline-evil-tag-face)
                                                    'l)
                                     (powerline-raw (zoo/powerline-clock-in)
                                                    'zoo/powerline-clock-in-face
                                                    'l)
                                     (powerline-vc face2 'r)
                                     (powerline-raw (zoo/powerline-flycheck-status-text)
                                                    (zoo/powerline-flycheck-status-face)
                                                    'l)
                                     (powerline-raw "%*" nil 'l)
                                     (powerline-buffer-size nil 'l)
                                     (powerline-raw mode-line-mule-info nil 'l)
                                     (powerline-buffer-id nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r)
                                     (powerline-hud face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))


(zoo/powerline-theme)

(provide 'zoo-powerline)

;;; zoo-powerline.el ends here
