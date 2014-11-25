(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("haskell" (or (mode . haskell-mode)
                              (mode . cabal-mode)
                              (name . ".ghci")))
               ("org" (mode . org-mode))
               ("ruby" (or
                        (mode . ruby-mode)
                        (mode . enh-ruby-mode)))

               ("zoo-emacs" (name . "^\\zoo-.*\\.el$"))
               ("git"
                (or (name . "^\\*magit.*$")
                    (mode . diff-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))))))

(provide 'zoo-ibuffer)
