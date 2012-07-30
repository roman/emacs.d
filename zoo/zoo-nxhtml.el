(if (boundp 'image-types)
    nil
  (defvar image-types nil))
(load "~/.emacs.d/el-get/nxhtml/autostart.el")
(setq mumamo-background-colors nil)
(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . eruby-html-mumamo-mode))

(provide 'zoo-nxhtml)
