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
    ;;(progn
    ;;  ;; Emacs 24+ includes ELPA, but requires some extra setup
    ;;  ;; to use the (better) mermelade repo
    ;;  (package-initialize)
    ;;  (if (>= emacs-major-version 24)
    ;;      (add-to-list 'package-archives
    ;;                   '("marmalade" . "http://marmalade-repo.org/packages/") t)))
  (install-elpa))

;;;;;;;;;;;;;;;;;;;;
;; el-get Config
;;;;;;;;;;;;;;;;;;;;

(defun install-el-get ()
  (eval-url
   "https://raw.github.com/dimitri/el-get/862dfe1025568b90c254dd91e054ea9bad5b319a/el-get-install.el"))

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
        (:name sunrise-commander
         :type git
         :url "https://github.com/escherdragon/sunrise-commander.git")

        (:name rinari
         :type git
         :url "https://github.com/eschulte/rinari.git")

        (:name autotest-mode
         :type git
         :url "https://github.com/BirdseyeSoftware/autotest-mode")

        (:name evil-paredit
         :type git
         :url "https://github.com/roman/evil-paredit.git")

        (:name dash
         :type git
         :url "https://github.com/magnars/dash.el.git")

        (:name edn.el
         :type git
         :url "https://github.com/BirdseyeSoftware/edn.el")

        (:name s
         :type git
         :url "https://github.com/magnars/s.el.git")

        (:name emacs-websocket
         :type git
         :url "https://github.com/ahyatt/emacs-websocket.git")

        (:name flip-tables
         :type http
         :url "http://www.emacswiki.org/emacs/download/flip-tables.el")

        (:name rainbow-mode
         :type git
         :url "https://github.com/emacsmirror/rainbow-mode.git")

        (:name ac-nrepl
         :type git
         :url "https://github.com/purcell/ac-nrepl.git")

        (:name clojure-mode
         :type git
         :url "https://github.com/tavisrudd/clojure-mode.git")

        (:name cljsbuild-mode
         :type git
         :url "https://github.com/BirdseyeSoftware/cljsbuild-mode.git")

        (:name rainbow-delimiters
         :type git
         :url "https://github.com/jlr/rainbow-delimiters.git")

        (:name evil-surround
         :type git
         :url "https://github.com/timcharper/evil-surround.git")

        (:name window-numbering
         :type git
         :url "https://github.com/nschum/window-numbering.el.git")

        (:name golden-ratio
         :type git
         :url "https://github.com/roman/golden-ratio.el.git")

        (:name winner-mode
         :type emacswiki)

        (:name helm
         :type git
         :url "https://github.com/emacs-helm/helm.git")

        (:name yasnippet
         :type git
         :url "https://github.com/emacsmirror/yasnippet.git")

        (:name helm-c-yasnippet
         :type git
         :url "https://github.com/emacs-helm/helm-c-yasnippet.git")

        (:name haskell-mode
         :type git
         :url "https://github.com/haskell/haskell-mode.git")

        (:name ghc-mod
         :type git
         :url "https://github.com/kazu-yamamoto/ghc-mod.git")

        (:name bm
         :type http
         :url "http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el")

        (:name lineker
         :type http
         :url "http://www.helsinki.fi/~sjpaavol/programs/lineker.el")

        (:name multi-term-ext
         :type git
         :url "https://github.com/roman/multi-term-ext")

        (:name popup
         :type git
         :url "https://github.com/emacsmirror/popup.git")

        (:name auto-complete
         :type git
         :url "https://github.com/emacsmirror/auto-complete.git")

        (:name color-theme-sunburst
         :type git
         :url "https://github.com/roman/Emacs-Sunburst-Color-Theme.git")))

;;;;;;;;;;;;;;;;;;;;
;;- Setting my dependencies
;;;;;;;;;;;;;;;;;;;;

(setq zoo-el-get-packages
      '(
       ; OH MEIN GOT! I can't live without dependencies
       package
       evil
       evil-surround
       evil-paredit
       paredit
       magit
       multi-term
       multi-term-ext
       ack
       sunrise-commander
       window-numbering
       winner-mode
       bm
       flymake-cursor
       emacs-websocket
       lineker
       goto-last-change
       smex
       golden-ratio
       popup
       auto-complete
       moz-repl
       notify
       autotest-mode

       edn.el
       dash
       s


       ; Lispy languages
       rainbow-mode
       rainbow-delimiters

       ; Clojure mode extensions
       clojure-mode
       swank-clojure
       cljsbuild-mode

       ;; Puppet mode extensions
       puppet-mode

       ; Haskell mode extensions
       haskell-mode
       ghc-mod

       ;; Ruby mode extensions
       ruby-mode
       ruby-compilation
       inf-ruby
       ri
       flymake-ruby
       ruby-block
       ruby-end

       ;; Rails mode extensions
       rinari
       rspec-mode

       ;; Color themes dependencies
       color-theme
       color-theme-sunburst
       ;;color-theme-solarized
       ))

(el-get 'sync zoo-el-get-packages)

(provide 'zoo-dependencies)
