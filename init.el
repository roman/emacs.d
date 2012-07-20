; This configuration file is inspired on Tavis Rudd's emacs.d, and
; Sebastian Cevey blogpost, both of these can be found at:
;
; * http://github.com/tavisrudd/emacs.d
; * http://bytes.inso.cc/2011/08/13/auto-installing-packages-in-emacs-with-elpa-and-el-get/

;;- Util functions

; Utility function derived from ELPA installation
; +info: http://tromey.com/elpa/install.html
;
(defun eval-url (url)
  (let ((buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (eval-region (point) (point-max))
      (kill-buffer (current-buffer)))))

;;- ELPA Config

(defun install-elpa ()
  (eval-url "http://tromey.com/elpa/package-install.el"))

(add-to-list 'load-path "~/.emacs.d/elpa")

(if (require 'package nil t)
    (progn
      ;; Emacs 24+ includes ELPA, but requires some extra setup
      ;; to use the (better) mermelade repo
      (if (>= emacs-major-version 24)
        (add-to-list 'package-archives
                     '("marmalade" . "http://marmalade-repo.org/packages/") t))
      (package-initialize))
  (install-elpa))

;;- el-get Config

(defun install-el-get ()
  (eval-url
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

; Ensure installation of el-get, if not there automatically
; install it
(unless (require 'el-get nil t)
  (install-el-get))

;;- Adding Extra sources
(setq el-get-generate-autoloads t
      el-get-sources '(
        (:name rinari
         :type git
         :url "https://github.com/eschulte/rinari.git")

        (:name flip-tables
         :type http
         :url "http://www.emacswiki.org/emacs/download/flip-tables.el")

        (:name rainbow-mode
         :type git
         :url "https://github.com/emacsmirror/rainbow-mode.git")

        (:name rainbow-delimiters
         :type git
         :url "https://github.com/jlr/rainbow-delimiters.git")

        (:name color-theme-sunburst
         :type git
         :url "https://github.com/roman/Emacs-Sunburst-Color-Theme.git")))

;;- Setting up my dependencies

(setq my-el-get-packages
      '(
       ; OH MEIN GOT! I can't live without dependencies
       package
       evil
       paredit
       magit

       ; Lispy languages
       rainbow-mode
       rainbow-delimiters

       ; Ruby mode extensions
       ruby-mode
       ruby-compilation
       inf-ruby
       flymake-ruby

       ; Rails mode extensions
       rinari
       rspec-mode

       ; Color themes dependencies
       color-theme
       color-theme-sunburst
       color-theme-almost-monokai
       color-theme-railscasts
       color-theme-solarized
       ))

(el-get 'wait my-el-get-packages)

;;- Configuration

;; Enable modes by default
(ido-mode 1)
(evil-mode 1)
(global-whitespace-mode 1)


;; Please do not create backup files
(setq make-backup-files nil)

;; Allow version control info on modline
(vc-mode 1)

;; Remove stupid menubar from the top
(menu-bar-mode 0)

;; Specify sunburst theme

(require 'color-theme-sunburst)
(color-theme-sunburst)

;; Accept utf-8 characters on the terminal

(set-terminal-coding-system 'utf-8-unix)

;; Whitespace/Tabs customization

; the tab-width is 4 by default
(setq tab-width 4)

; don't highlight lines with 8 spaces or more at the start
(setq indent-tabs-mode nil)


; whitespace-mode should only show this properties
(setq whitespace-style
  '(tabs spaces trailing lines space-before-tab
    newline empty space-after-tab
    space-mark tab-mark newline-mark))


; Special characters for newline and tabs
(setq whitespace-display-mappings
  '(
     (space-mark ?\ [? ])
     (newline-mark ?\n [?\u00AC ?\n])
     (tab-mark ?\t [?\u25B8 ?\t])
   ))

; special colores for newline and tab character
(custom-set-faces
 '(whitespace-space
   ((((class color) (background dark))
     (:background "#111" :foreground "white"))
    (((class color) (background light))
     (:background "yellow" :foreground "black"))
    (t (:inverse-video t))))
 '(whitespace-newline
   ((((class color) (background dark))
     (:background "#111" :foreground "#111"))))
 '(whitespace-tab
   ((((class color) (background dark))
     (:background "#111" :foreground "#111")))))


;; on shells, please handle properly the ansi escape codes
(add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook    'ansi-color-for-comint-mode-on)
