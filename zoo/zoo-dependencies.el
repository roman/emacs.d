;; This dependencies module is inspired on Tavis Rudd's emacs.d, and
;; Sebastian Cevey blogpost, both of these can be found at:
;;
;; * http://github.com/tavisrudd/emacs.d
;; * http://bytes.inso.cc/2011/08/13/auto-installing-packages-in-emacs-with-elpa-and-el-get/

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Utility function derived from ELPA installation
;; +info: http://tromey.com/elpa/install.html
;;

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
  (progn
    ;; Emacs 24+ includes ELPA, but requires some extra setup
    ;; to use the (better) mermelade repo
    (package-initialize)
    (if (>= emacs-major-version 24)
        (add-to-list 'package-archives
                     '("marmalade" . "http://marmalade-repo.org/packages/") t)))
  (install-elpa))

;;;;;;;;;;;;;;;;;;;;
;; el-get Config
;;;;;;;;;;;;;;;;;;;;

(defun install-el-get ()
  (eval-url
   "http://raw.github.com/dimitri/el-get/862dfe1025568b90c254dd91e054ea9bad5b319a/el-get-install.el"))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; Ensure installation of el-get, if not there automatically
;; install it
(unless (require 'el-get nil t)
  (install-el-get))



;;;;;;;;;;;;;;;;;;;;
;;  Adding Extra sources
;;;;;;;;;;;;;;;;;;;;

(setq el-get-generate-autoloads t
      el-get-sources '(

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        (:name golden-ratio
         :type git
         :url "http://github.com/roman/golden-ratio.el.git")

        (:name navorski.el
         :type git
         :url  "ssh://git@bitbucket.org/romanandreg/navorski.el.git")

        (:name birdseye
         :type git
         :url "ssh://git@bitbucket.org/romanandreg/birdseye.el.git")

        (:name proctor-mode
         :type git
         :url "ssh://git@bitbucket.org/romanandreg/proctor-mode.git")

        (:name tracker.el
         :type git
         :url "http://github.com/BirdseyeSoftware/tracker.el.git")

        (:name evil-paredit
         :type git
         :url "http://github.com/roman/evil-paredit.git")

        (:name edn.el
         :type git
         :url "https://github.com/BirdseyeSoftware/edn.el")

        (:name sisyphus-mode
         :type git
         :url "ssh://git@bitbucket.org/romanandreg/sisyphus-mode.git")

        (:name projectile
         :type git
         :url "https://github.com/bbatsov/projectile.git")

        (:name ace-jump-mode
         :type git
         :url "https://github.com/winterTTr/ace-jump-mode.git")

        (:name anzu
         :type git
         :url "https://github.com/syohex/emacs-anzu.git")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        (:name s
         :type git
         :url "http://github.com/magnars/s.el.git")

        (:name f
         :type git
         :url "https://github.com/rejeep/f.el.git")

        (:name dash
         :type git
         :url "http://github.com/magnars/dash.el.git")

        (:name git-gutter
         :type git
         :url "https://github.com/syohex/emacs-git-gutter.git")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        (:name scss-mode
         :type git
         :url "https://github.com/antonj/scss-mode.git")

        (:name less-css-mode
         :type git
         :url "https://github.com/purcell/less-css-mode.git")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        (:name coffee-mode
         :type git
         :url "http://github.com/defunkt/coffee-mode.git")

        (:name coffeelintmode
         :type git
         :url "http://github.com/ajkavanagh/coffeelintnode.git")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        (:name rinari
         :type git
         :url "http://github.com/eschulte/rinari.git")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



        (:name ac-nrepl
         :type git
         :url "http://github.com/purcell/ac-nrepl.git")

        (:name clojure-mode
         :type git
         :url "https://github.com/technomancy/clojure-mode.git")

        (:name rainbow-delimiters
         :type git
         :url "http://github.com/jlr/rainbow-delimiters.git")

        (:name nrepl.el
         :type git
         :url "https://github.com/kingtim/nrepl.el.git")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


        (:name haskell-mode
         :type git
         :url "http://github.com/haskell/haskell-mode.git")

        (:name ghc-mod
         :type git
         :url "http://github.com/kazu-yamamoto/ghc-mod.git")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        (:name pkg-info
         :type git
         :url "https://github.com/lunaryorn/pkg-info.el.git")

        (:name epl
         :type git
         :url "https://github.com/cask/epl.git")

        (:name sunrise-commander
         :type git
         :url "http://github.com/escherdragon/sunrise-commander.git")

        (:name emacs-websocket
         :type git
         :url "https://github.com/ahyatt/emacs-websocket.git")

        (:name rainbow-mode
         :type git
         :url "http://github.com/emacsmirror/rainbow-mode.git")

        (:name evil-surround
         :type git
         :url "http://github.com/timcharper/evil-surround.git")

        (:name window-numbering
         :type git
         :url "http://github.com/nschum/window-numbering.el.git")

        (:name winner-mode
         :type emacswiki)

        (:name helm
         :type git
         :url "http://github.com/emacs-helm/helm.git")

        (:name yasnippet
         :type git
         :url "https://github.com/capitaomorte/yasnippet.git")

        (:name helm-c-yasnippet
         :type git
         :url "http://github.com/emacs-helm/helm-c-yasnippet.git")

        (:name bm
         :type http
         :url "http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el")

        ;; (:name solarized-emacs
        ;;  :type git
        ;;  :url "https://github.com/bbatsov/solarized-emacs.git")

        (:name flycheck
         :type git
         :url "https://github.com/flycheck/flycheck.git")

        (:name flycheck-color-mode-line
         :type git
         :url "https://github.com/flycheck/flycheck-color-mode-line.git")

        (:name lineker
         :type http
         :url "http://www.helsinki.fi/~sjpaavol/programs/lineker.el")

        (:name popup
         :type git
         :url "http://github.com/emacsmirror/popup.git")

	(:name magit
	 :type git
	 :url "https://github.com/magit/magit.git")

        (:name auto-complete
         :type git
         :url "http://github.com/emacsmirror/auto-complete.git")))

;;;;;;;;;;;;;;;;;;;;
;;- Setting my dependencies
;;;;;;;;;;;;;;;;;;;;

(setq zoo-el-get-packages
      '(
                                        ; OH MEIN GOT! I can't live without dependencies
        package
        birdseye

        s
        dash
        f

        evil
        evil-surround
        evil-paredit

        multi-term
        navorski.el
        proctor-mode

        scss-mode
        less-css-mode

        git-gutter

        coffee-mode
        coffeelintmode

	anzu
        tracker.el
        paredit
        hlinum
        magit
        ack
        sunrise-commander
        window-numbering
        winner-mode
        bm
        emacs-websocket
        lineker
        goto-last-change
        smex
        golden-ratio
        popup
        auto-complete
        moz-repl
        notify
        iedit
        rainbow-mode
        flycheck
        flycheck-color-mode-line
        pkg-info
        epl
        helm
        projectile
        ace-jump-mode
        yasnippet


        ;; Clojure
        clojure-mode
        rainbow-delimiters
        edn.el
        ;; swank-clojure
        sisyphus-mode
        nrepl.el

        ;; Puppet
        puppet-mode

        ;; Haskell
        haskell-mode
        ghc-mod

        ;; Ruby
        ruby-mode
        ruby-compilation
        inf-ruby
        ri
        ruby-block
        ruby-end

        ;; Rails
        rinari
        rspec-mode

        ;; Color Themes
        color-theme))

(el-get 'sync zoo-el-get-packages)

(provide 'zoo-dependencies)
