; This dependencies module is inspired on Tavis Rudd's emacs.d, and
; Sebastian Cevey blogpost, both of these can be found at:
;
; * http://github.com/tavisrudd/emacs.d
; * http://bytes.inso.cc/2011/08/13/auto-installing-packages-in-emacs-with-elpa-and-el-get/

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

;;;;;;;;;;;;;;;;;;;;
;; ELPA Config
;;;;;;;;;;;;;;;;;;;;

(defun install-elpa ()
  (eval-url "http://tromey.com/elpa/package-install.el"))

(add-to-list 'load-path "~/.emacs.d/elpa")

(if (require 'package nil t)
  (package-initialize)
    ;(progn
    ;  ;; Emacs 24+ includes ELPA, but requires some extra setup
    ;  ;; to use the (better) mermelade repo
    ;  (package-initialize)
    ;  (if (>= emacs-major-version 24)
    ;      (add-to-list 'package-archives
    ;                   '("marmalade" . "http://marmalade-repo.org/packages/") t)))
  (install-elpa))

;;;;;;;;;;;;;;;;;;;;
;; el-get Config
;;;;;;;;;;;;;;;;;;;;

(defun install-el-get ()
  (eval-url
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

; Ensure installation of el-get, if not there automatically
; install it
(unless (require 'el-get nil t)
  (install-el-get))

;;;;;;;;;;;;;;;;;;;;
;;  Adding Extra sources
;;;;;;;;;;;;;;;;;;;;

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

        (:name clojure-mode
         :type git
         :url "https://github.com/technomancy/clojure-mode.git")

        (:name rainbow-delimiters
         :type git
         :url "https://github.com/jlr/rainbow-delimiters.git")

        (:name evil-surround
         :type git
         :url "https://github.com/timcharper/evil-surround.git")

        (:name color-theme-sunburst
         :type git
         :url "https://github.com/roman/Emacs-Sunburst-Color-Theme.git")

        (:name nxhtml
         :type git
         :url "https://github.com/emacsmirror/nxhtml.git")))

;;;;;;;;;;;;;;;;;;;;
;;- Setting my dependencies
;;;;;;;;;;;;;;;;;;;;

(setq zoo-el-get-packages
      '(
       ; OH MEIN GOT! I can't live without dependencies
       package
       evil
       evil-surround
       paredit
       magit
       multi-term
       ack

       ; Lispy languages
       rainbow-mode
       rainbow-delimiters

       ; Clojure mode extensions
       clojure-mode
       swank-clojure

       ; Haskell mode extensions
       haskell-mode

       ; Ruby mode extensions
       ruby-mode
       ruby-compilation
       inf-ruby
       ri
       flymake-ruby
       nxhtml

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

(el-get 'wait zoo-el-get-packages)

(provide 'zoo-dependencies)
