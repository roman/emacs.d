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

(unless (require 'el-get nil t)
  (install-el-get))

;;- Adding Extra sources

; Ensure installation of el-get, if not there automatically
; install it
(setq el-get-generate-autoloads t
      el-get-sources '(
	(:name rinari
	 :type git
	 :url "https://github.com/eschulte/rinari.git")

	(:name flip-tables
	 :type http
	 :url "http://www.emacswiki.org/emacs/download/flip-tables.el")

	(:name color-theme-sunburst
	 :type git
	 :url "https://github.com/roman/Emacs-Sunburst-Color-Theme.git")))

;;- Setting up my dependencies

(setq my-el-get-packages
      '(
       ; OH MY GOT! I can't live without dependencies
       package
       evil
       paredit
       magit

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

;;- Please do not create backup files
(setq make-backup-files nil)

;;- Enable modes by default

(ido-mode 1)
(evil-mode 1)

;;- Allow version control info on modline
(vc-mode 1)

;;- Remove stupid menubar from the top
(menu-bar-mode 0)

;;- Specify sunburst theme

(require 'color-theme-sunburst)
(color-theme-sunburst)

;;- On shells, please handle properly the ansi escape codes
(add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


