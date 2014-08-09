;;; lineker.el --- minor mode that warns about too long lines

;;; Author: Santeri Paavolainen <santtu@iki.fi>
;;; Maintainer: Santeri Paavolainen <santtu@iki.fi>
;;; Creation-Date: Mon Oct  2 13:17:36 2000 EEST

;;; Time-stamp: Fri Dec 14 09:46:35 2007 <santtu@iki.fi>

;;; Changelog:

;; 2007-12-14 Santeri Paavolainen
;;   Integrated Lennart's changes into version control and cleaned up some
;;   of my own silly goofs at the same time :-) Thanks Lennart for the
;;   Emacs 22 changes! Incremented version number to 1.7 too.

;; 2007-12-13 Lennart Borgman
;;   Modified for Emacs 22. Corrected bug that deleted all overlays in
;;   an overlong line.

;;; Copyright (c) 2000 SSH Communications Security, Finland
;;; All rights reserved

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.

;;; You use this mode by putting adding a call to the `lineker-mode'
;;; in appropriate require and hook in your `.emacs' file, for
;;; example:

;;;     (require 'lineker)
;;;     (add-hook 'c-mode-hook 'lineker-mode)

;;; Thereafter the C-mode automatically gets Lineker kicking you
;;; overlong lines to little pieces! You can also use `lineker-mode'
;;; command to toggle the mode on and off.

;;; There are a few customizable variables, but the defaults should be
;;; reasonable.

;;; To move to next/previous overlong line in `lineker-mode' you can
;;; use M-n/M-p (similar to the bindings for `next-error' /
;;; `previous-error' in `compilation-mode'.

;;; This program has been developed under XEmacs 21.1 (currently I'm
;;; using 21.5). It should work under GNU Emacs, though...

;;; Oh. What this program actually does? Well, while you edit it will
;;; highlight any overlong lines (as defined by
;;; `lineker-column-limit''), as well as check for overlong lines when
;;; saving the buffer and asking the user whether they really want to
;;; save the buffer. I find this program to be very useful when I'm
;;; editing C files at work, since 1) I use proportional font (let's
;;; not get a war on that, shall we?), and 2) the company policy
;;; requires all source files to be formattable on 80-character width
;;; display/paper/etc.

;;; Because of the proportional font I can't see over-80 lines so
;;; easily, and so previously I got some flames from other
;;; developers... No longer! Since the mode highlight too long lines
;;; when turned on, I can now flame other developers about too long
;;; lines in their code :-)

;; Provide:
(provide 'lineker)

(defvar lineker-version "1.7"
  "Version string for Lineker.")

;;; Customization:

(defgroup lineker nil
  "Line-length checking on the fly."
  :tag "Lineker"
  :prefix "lineker-"
  :group 'editing)

(defcustom lineker-column-limit 79
  "The column limit after which lineker starts producing warnings."
  :group 'lineker
  :type 'number)

(defcustom lineker-warning-beep t
  "Whether to beep (once) when the column limit is exceeded."
  :group 'lineker
  :type 'boolean)

(defcustom lineker-check-on-save t
  "Check the whole buffer for overlong lines when saving."
  :group 'lineker
  :type 'boolean)

(defface lineker-warning-face
  '((((type x)) (:background "HotPink"))
    (t (:bold t :inverse-video t)))
  "Face used for marking a overlong lines in Lineker minor mode."
  :group 'lineker)

;;; Code:

(defvar lineker-emacs
  (cond ((string-match "XEmacs" emacs-version) 'xemacs)
        (t 'hapatusmacs))
  "Type of the Emacs we're running in.")

(defmacro lineker-if-xemacs (with withnot)
  (declare (indent 1) (debug t))
  (if (eq lineker-emacs 'xemacs)
      with
    withnot))

(unless (fboundp 'point-at-eol)
  (defun point-at-eol ()
    (save-excursion
      (end-of-line)
      (point)))
  (defun point-at-bol ()
    (save-excursion
      (beginning-of-line)
      (point))))

(defvar lineker-current-highlights nil)
(make-variable-buffer-local 'lineker-current-highlights)

(defun lineker-remove-all-highlights ()
  "Remove all highlights from the current buffer."
  (let ((highlights))
    (lineker-if-xemacs
     (maphash (lambda (key value)
		(setq highlights (cons key highlights)))
	      lineker-current-highlights)
     (setq highlights lineker-current-highlights))
    (mapcar (lambda (highlight)
              (lineker-remove-highlight highlight)) highlights)))

(defun lineker-replace-all-highlights (highlights)
  "Replace hightlights of the current buffer with the given highlights."
  (lineker-remove-all-highlights)
  (mapcar (lambda (highlight)
            (lineker-add-highlight highlight)) highlights))

(defun lineker-remove-highlight (highlight)
  "Removes a highlighted highlight."
  (lineker-if-xemacs
   ;; Simple delete-extent will works, as the has where the
   ;; extent/highlight is referenced from is a weak hash
   (delete-extent highlight)
   ;; For GNU Emacs, remove the highlight explicitly from the
   ;; highlight list.
   (progn
     (delete-overlay highlight)
     (setq lineker-current-highlights
	   (delq highlight lineker-current-highlights)))))

(defun lineker-add-highlight (highlight)
  "Adds a highlighted highlight."
  (lineker-if-xemacs
   (progn
     (puthash highlight t lineker-current-highlights)
     (set-extent-face highlight (cons 'lineker-warning-face
				      (extent-face highlight))))
   (progn
     (setq lineker-current-highlights
	   (cons highlight lineker-current-highlights))
     (overlay-put highlight 'face 'lineker-warning-face))))

(defun lineker-our-highlight-p (highlight)
  (lineker-if-xemacs
   (gethash highlight lineker-current-highlights)
   (overlay-get highlight 'lineker)))

(defun lineker-highlight-position (highlight)
  (lineker-if-xemacs
   (extent-start-position highlight)
   (overlay-start highlight)))

(defun lineker-make-current-line-highlight ()
  "Make an highlight that covers the over-long part of the current line."
  (save-excursion
    (let ((begin (progn (beginning-of-line)
                        (move-to-column lineker-column-limit) (point)))

          (end (progn (end-of-line) (point))))
      (lineker-if-xemacs
       (make-extent begin end)
       (let ((ovl (make-overlay begin end)))
	 (overlay-put ovl 'lineker t)
	 ovl)))))

(defun lineker-get-current-line-highlight ()
  (map-extents
   (lambda (highlight ignored)
     (lineker-our-highlight-p highlight))
   (current-buffer) (point-at-bol) (point-at-eol)))

(defun lineker-highlight-current-line ()
  ;; If there was no previous highlight, then warn the beep (if
  ;; so). If previous highlight was, return its face to original.
  (lineker-add-highlight (lineker-make-current-line-highlight)))

(defun lineker-check-current-line ()
  (> (save-excursion (goto-char (point-at-eol)) (current-column))
     lineker-column-limit))

(defun lineker-after-change-function (start stop len)
  "Do not call this function directly.

Checks the Lineker column limit, and performs the highlight action if
necessary."
  (interactive)

  ;; First, map the minimum and maximum of all those highlights that
  ;; overlap the changed region. This is necessary so we can update
  ;; all potential line length violations (think about "delete" and
  ;; "open-line").

  (setq start (save-excursion (goto-char start) (point-at-bol))
        stop (save-excursion (goto-char stop) (forward-line 1)
                             (point-at-eol)))

  (let ((estart (point-max))
        (eend (point-min))
        (matches nil)
        (change 0))
    (lineker-if-xemacs
     (map-extents
      (lambda (highlight arg)
	(if (lineker-our-highlight-p highlight)
	    (setq estart (min estart (extent-start-position highlight))
		  eend (max eend (extent-end-position highlight))
		  change (1- change)
		  matches (cons highlight matches)))
	nil)
      (current-buffer)
      ;; We go through the change region *plus* the to the end of next
      ;; line. This way we automatically handle open-line (since we
      ;; always consider the "next" line also).
      start stop nil 'all-extents-closed)

     (mapcar (lambda (highlight)
	       (if (lineker-our-highlight-p highlight)
		   (setq estart (min estart (overlay-start highlight))
			 eend (max eend (overlay-end highlight))
			 change (1- change)
			 matches (cons highlight matches))))
	     (overlays-in start stop)))

    ;; Now we know the proper start and end points. Remove all highlights
    ;; in that area then and re-evaluate the region.
    (mapcar (lambda (highlight)
              (lineker-remove-highlight highlight))
            matches)

    ;; If there was *no* old highlights in the area, we better make sure
    ;; estart and eend match at least start and stop.
    (setq estart (min estart start)
          eend (max eend stop))

    ;; Now go through the region, line-by-line.
    (save-excursion
      (goto-char estart)
      (while (< (point) eend)
        (if (lineker-check-current-line)
            (progn
              (lineker-highlight-current-line)
              (setq change (1+ change))))
        (forward-line 1)))

    ;; Beep only *once* when we go over the limit. This also means
    ;; (which is good) that joining two lines of either one is already
    ;; "overlong" will not cause a beep.
    (if (and (> change 0)
             lineker-warning-beep)
        (beep t))))

(defun lineker-find-overlong-lines ()
  "Return a list of all overlong lines in the current buffer."
  (let ((error-highlights nil))
    (save-excursion
      (beginning-of-buffer)
      (while (< (point) (point-max))
        (if (lineker-check-current-line)
            ;; Save first error as highlight.
            (setq error-highlights
                  (cons (lineker-make-current-line-highlight)
                        error-highlights)))
        (forward-line 1)))
    error-highlights))

(defun lineker-write-contents-hook ()
  (if lineker-check-on-save
      (let ((error-highlights (lineker-find-overlong-lines)))

        ;; If there was errors, highlight the first overlong line
        ;; and ask the user whether to continue with the save or not.

        (unless (null error-highlights)
          ;; Remove all old highlights, and replace them with the
          ;; new. This is done just in case we've messed up some
          ;; highlights before..
          (lineker-replace-all-highlights error-highlights)
          (setq error-highlights (lineker-find-overlong-lines))

          (when error-highlights
            ;; Move the point to one of the problem points, and ask
            ;; the user wheteher to continue save.
            (goto-char (lineker-highlight-position (car error-highlights)))
            (not (y-or-n-p "Long lines detected -- continue save? ")))))))

(defun lineker-next-overlong-line ()
  "Move to next overlong line."
  (interactive)
  (lineker-move-to-overlong-line 1))

(defun lineker-previous-overlong-line ()
  "Move to previous overlong line."
  (interactive)
  (lineker-move-to-overlong-line -1))

(defun lineker-move-to-overlong-line (&optional arg)
  "Move to overlong line.
Move point forward (or backward, if argument is negative) to
next (or ARGth) overlong line (see `lineker-column-limit'). This
command does not require `lineker-mode' to be enabled to work."
  (interactive "p")
  (unless arg (setq arg 1))
  (let ((direction (cond ((> arg 0) 1) (t -1)))
        (steps (abs arg))
        (here (point)))
    ;; This will either terminate when we're traversen the requested
    ;; number of overlong lines, or until we hit either beginning or
    ;; end of the buffer.
    (while (and (> steps 0)
                (= (forward-line direction) 0)
                (/= (point) (point-max)))
      ;; Overlong line?
      (when (lineker-check-current-line)
        (setq here (point))
        (setq steps (1- steps))))
    ;; Now move to the limit point on this line, if we've finished
    ;; with the correct number of steps.. or ring bell if we are at a
    ;; buffer beginning/end.
    (if (= steps 0)
        (move-to-column lineker-column-limit)
      (goto-char here)
      (beep)
      (message "No more overlong line in this direction"))))

(defvar lineker-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Similar to next-error etc in `compilation-mode':
    (define-key map [(meta ?n)] 'lineker-next-overlong-line)
    (define-key map [(meta ?p)] 'lineker-previous-overlong-line)
    map))

(define-minor-mode lineker-mode
  "Toggle Lineker minor mode.
With arg, turn Lineker mode on iff arg is positive.

When Lineker mode is enabled it will both highlight overlong lines (as
defined by `lineker-column-limit') and warn if you try to save a file
with overlong lines (`lineker-check-on-save').

Very handy for source code editing.

See also `lineker-next-overlong-line' function."

  :lighter " Lineker"

  (if lineker-mode
      ;; Turn on.
      (progn
        (add-hook 'after-change-functions 'lineker-after-change-function nil t)
        (add-hook 'write-contents-hooks 'lineker-write-contents-hook nil t)
        (setq lineker-current-highlights
              (lineker-if-xemacs
	       (make-weak-hashtable 1)
	       nil))
        (let ((highlights (lineker-find-overlong-lines)))
          (if highlights
              (lineker-replace-all-highlights highlights)))
        (run-hooks 'lineker-mode-hook))
    ;; Turn off.
    (progn
      (remove-hook 'after-change-functions 'lineker-after-change-function t)
      (remove-hook 'write-contents-hooks 'lineker-write-contents-hook t)
      (lineker-remove-all-highlights)
      )))

(defun turn-on-lineker ()
  "Unconditionally turn on Lineker mode."
  (lineker-mode 1))

(defun turn-off-lineker ()
  "Unconditionally turn off Lineker mode."
  (lineker-mode 0))

;; lineker.el ends here.
