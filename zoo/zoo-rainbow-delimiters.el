(require 'rainbow-delimiters nil)

(set-face-background 'rainbow-delimiters-unmatched-face "red")
(set-face-foreground 'rainbow-delimiters-depth-1-face "#E52020")
(set-face-foreground 'rainbow-delimiters-depth-2-face "#68A8FF")
(set-face-foreground 'rainbow-delimiters-depth-3-face "#FA2473")
(set-face-foreground 'rainbow-delimiters-depth-4-face "#A6E12B")
(set-face-foreground 'rainbow-delimiters-depth-5-face "#FB951D")
(set-face-foreground 'rainbow-delimiters-depth-6-face "#FF3040")
(set-face-foreground 'rainbow-delimiters-depth-7-face "#0040FF")
(set-face-foreground 'rainbow-delimiters-depth-8-face "#A000FF")
(set-face-foreground 'rainbow-delimiters-depth-9-face "#00FF80")

(defun zoo/turn-on-rainbow-delimiters ()
  (interactive)
  (rainbow-delimiters-mode 1))

(provide 'zoo-rainbow-delimiters)
