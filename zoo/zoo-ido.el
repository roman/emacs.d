;;;;;;;;;;;;;;;;;;;;
;; IDO config
;;;;;;;;;;;;;;;;;;;;

;; shamelessly stoled from tavisrudd's
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
; Don't do stupid shit like changing the working directory
; while I'm creating new files
(setq ido-auto-merge-work-directories-length -1)

;; from http://emacs-fu.blogspot.com/2009_02_01_archive.html
(setq
  ido-ignore-buffers '("\\` " "^\*Back" ".*Completions\*" "^\*Ido" "^\*trace"
                       "^\*compilation" "^\*GTAGS" "^session\.*")
  ido-case-fold t
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-file-list 50 ; remember many
  ido-use-filename-at-point nil
  ido-use-url-at-point nil
  ido-max-prospects 8 ; don't spam my minibuffer
  ido-confirm-unique-completion t ; wait for RET, even with unique completion
  )

(provide 'zoo-ido)