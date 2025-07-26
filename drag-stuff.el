;;; drag-stuff.el --- Drag stuff (lines, words, region, etc...) around  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2016 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.3.0
;; Keywords: speed, convenience
;; URL: http://github.com/rejeep/drag-stuff

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; drag-stuff is a minor mode for dragging stuff around in Emacs. You
;; can drag lines, words and region.

;; To use drag-stuff, make sure that this file is in Emacs load-path
;; (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require drag-stuff
;; (require 'drag-stuff)

;; To start drag-stuff
;; (drag-stuff-mode t) or M-x drag-stuff-mode
;;
;; drag-stuff is buffer local, so hook it up
;; (add-hook 'ruby-mode-hook 'drag-stuff-mode)
;;
;; Or use the global mode to activate it in all buffers.
;; (drag-stuff-global-mode t)

;; Drag Stuff stores a list (`drag-stuff-except-modes') of modes in
;; which `drag-stuff-mode' should not be activated in (note, only if
;; you use the global mode) because of conflicting use.
;;
;; You can add new except modes:
;;   (add-to-list 'drag-stuff-except-modes 'conflicting-mode)

;; Default modifier key is the meta-key. This can be changed and is
;; controlled by the variable `drag-stuff-modifier'.
;;
;; Control key as modifier:
;;   (setq drag-stuff-modifier 'control)
;;
;; Meta and Shift keys as modifier:
;;   (setq drag-stuff-modifier '(meta shift))

;;; Code:

(defgroup drag-stuff nil
  "Drag stuff (lines, words, region, etc...) in buffers"
  :group 'files
  :prefix "drag-stuff-")

(defvar drag-stuff-except-modes ()
  "A list of modes in which `drag-stuff-mode' should not be activated.")

(defvar drag-stuff-modifier 'meta
  "Modifier key(s) for bindings in `drag-stuff-mode-map'.")

(defvar drag-stuff-mode-map (make-sparse-keymap)
  "Keymap for `drag-stuff-mode'.")

(defvar drag-stuff-before-drag-hook nil
  "Called before dragging occurs.")

(defvar drag-stuff-after-drag-hook nil
  "Called after dragging occurs.")

(defun drag-stuff--copy-region (beg end)
  "Copy region with text properties"
  (buffer-substring beg end))

(defun drag-stuff--copy-region-noprops (beg end)
  "Copy region with text properties"
  (buffer-substring-no-properties beg end))

(defvar drag-stuff--copy-region #'drag-stuff--copy-region-noprops
  "Function used to copy text from region.")

;; save-mark-and-excursion in Emacs 25 works like save-excursion did before
(eval-when-compile
  (when (not (fboundp #'save-mark-and-excursion))
    (defmacro save-mark-and-excursion (&rest body)
      `(save-excursion ,@body))))

(defun drag-stuff--kbd (key)
  "Key binding helper."
  (let ((mod (if (listp drag-stuff-modifier)
                 drag-stuff-modifier
               (list drag-stuff-modifier))))
    (vector (append mod (list key)))))

(defun drag-stuff--line-at-mark ()
  "Returns the line number where mark (first char selected) is."
  (line-number-at-pos (mark)))

(defun drag-stuff--line-at-point ()
  "Returns the line number where point (current selected char) is."
  (line-number-at-pos (point)))

(defun drag-stuff--col-at-mark ()
  "Returns the column number where mark (first char selected) is."
  (save-mark-and-excursion (exchange-point-and-mark) (current-column)))

(defun drag-stuff--col-at-point ()
  "Returns the column number where point (current selected char) is."
  (current-column))

;; visual-line-mode replaces longlines-mode since Emacs 23 ...
(defmacro drag-stuff--execute (&rest body)
  "Execute BODY without conflicting modes."
  `(let ((auto-fill-function nil)
         (electric-indent-mode nil))
     (run-hooks 'drag-stuff-before-drag-hook)
     ,@body
     (run-hooks 'drag-stuff-after-drag-hook)))

;;;###autoload
(defun drag-stuff-up (arg)
  "Drag stuff ARG lines up."
  (interactive "p")
  (drag-stuff--execute
   (if mark-active
       (drag-stuff-lines-up (- arg))
     (drag-stuff-line-up (- arg)))))

;;;###autoload
(defun drag-stuff-down (arg)
  "Drag stuff ARG lines down."
  (interactive "p")
  (drag-stuff--execute
   (if mark-active
       (drag-stuff-lines-down arg)
     (drag-stuff-line-down arg))))

;;;###autoload
(defun drag-stuff-right (arg)
  "Drag stuff ARG lines to the right."
  (interactive "p")
  (if mark-active
      (drag-stuff-region-right arg)
    (drag-stuff-word-right arg)))

;;;###autoload
(defun drag-stuff-left (arg)
  "Drag stuff ARG lines to the left."
  (interactive "p")
  (if mark-active
      (drag-stuff-region-left arg)
    (drag-stuff-word-left arg)))

(defun drag-stuff-line-up (arg)
  "Drag current line ARG lines up."
  (if (> (line-number-at-pos) (abs arg))
      (drag-stuff-line-vertically
       (lambda (beg end column)
         (drag-stuff-drag-region-up beg end arg)
         (move-to-column column)))
    (message "Can not move line further up")))

(defun drag-stuff-line-down (arg)
  "Drag current line ARG lines down."
  (if (<= (+ (line-number-at-pos) arg) (count-lines (point-min) (point-max)))
      (drag-stuff-line-vertically
       (lambda (beg end column)
         (drag-stuff-drag-region-down beg end arg)
         (move-to-column column)))
    (message "Can not move line further down")))

(defun drag-stuff-line-vertically (fn)
  "Yields variables used to drag line vertically."
  (let ((column (current-column))
        (beg (line-beginning-position))
        (end (line-end-position)))
    (funcall fn beg end column)))

(defun drag-stuff-lines-up (arg)
  "Move all lines in the selected region ARG lines up."
  (if (> (line-number-at-pos (region-beginning)) (abs arg))
      (drag-stuff-lines-vertically
       (lambda (beg end)
         (drag-stuff-drag-region-up beg end arg)))
    (message "Can not move lines further up")))

(defun drag-stuff-lines-down (arg)
  "Move all lines in the selected region ARG lines up."
  (let ((selection-end (region-end)))
    (if (<= (+ (line-number-at-pos selection-end) arg) (count-lines (point-min) (point-max)))
        (drag-stuff-lines-vertically
         (lambda (beg end)
           (drag-stuff-drag-region-down beg end arg)))
      (message "Can not move lines further down"))))

(defun drag-stuff-lines-vertically (fn)
  "Yields variables used to drag lines vertically."
  (let* ((mark-line (drag-stuff--line-at-mark))
         (point-line (drag-stuff--line-at-point))
         (mark-col (drag-stuff--col-at-mark))
         (point-col (drag-stuff--col-at-point))
         (bounds (drag-stuff-whole-lines-region))
         (beg (car bounds))
         (end (car (cdr bounds)))
         (deactivate-mark nil)
         (arg (or current-prefix-arg 1)))

    (funcall fn beg end)
    ;; Restore region
    (forward-line mark-line)
    (forward-line arg)
    (move-to-column mark-col)
    (exchange-point-and-mark)
    (forward-line point-line)
    (forward-line arg)
    (move-to-column point-col)))

(defun drag-stuff-drag-region-up (beg end arg)
  "Drags region between BEG and END ARG lines up."
  (let ((region (funcall drag-stuff--copy-region beg end)))    
    (delete-region beg end)
    (delete-char -1)
    (forward-line (+ arg 1))
    (goto-char (line-beginning-position))
    (insert region)
    (newline)
    (forward-line -1)))

(defun drag-stuff-drag-region-down (beg end arg)
  "Drags region between BEG and END ARG lines down."
  (let ((region (funcall drag-stuff--copy-region beg end)))
    (delete-region beg end)
    (delete-char 1)
    (forward-line (- arg 1))
    (goto-char (line-end-position))
    (newline)
    (insert region)))

(defun drag-stuff-whole-lines-region ()
  "Return the positions of the region with whole lines included."
  (let (beg end)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (list beg end)))

(defun drag-stuff-region-left (arg)
  "Drags region left ARG times."
  (if (> (min (point) (mark)) (point-min))
      (drag-stuff-region-horizontally (- arg))
    (message "Can not move region further to the left")))

(defun drag-stuff-region-right (arg)
  "Drags region right ARG times."
  (if (< (max (point) (mark)) (point-max))
      (drag-stuff-region-horizontally arg)
    (message "Can not move region further to the right")))

(defun drag-stuff-region-horizontally (arg)
  "Drags region horizontally ARG times."
  (let* ((beg (mark))
         (end (point))
         (region (buffer-substring-no-properties beg end))
         (deactivate-mark nil))
    (delete-region beg end)
    (forward-char arg)
    (insert region)
    (set-mark (+ beg arg))
    (goto-char (+ end arg))))

(defun drag-stuff-word-left (arg)
  "Drags word left ARG times."
  (drag-stuff-word-horizontally (- arg)))

(defun drag-stuff-word-right (arg)
  "Drags word right ARG times."
  (drag-stuff-word-horizontally arg))

(defun drag-stuff-word-horizontally (arg)
  "Drags word horizontally ARG times."
  (let ((old-point (point))
        (offset (- (save-mark-and-excursion (forward-word) (point)) (point))))
    (condition-case _err
        (progn
          (transpose-words arg)
          (backward-char offset))
      (error
       (message
        (if (> arg 0)
            "Can not move word further to the right"
          "Can not move word further to the left"))
       (goto-char old-point)))))

(defun drag-stuff-define-keys ()
  "Defines keys for `drag-stuff-mode'."
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'up) 'drag-stuff-up)
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'down) 'drag-stuff-down)
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'right) 'drag-stuff-right)
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'left) 'drag-stuff-left))

;;;###autoload
(define-minor-mode drag-stuff-mode
  "Drag stuff around."
  :init-value nil
  :lighter " drag")

;;;###autoload
(defun turn-on-drag-stuff-mode ()
  "Turn on `drag-stuff-mode'."
  (interactive)
  (unless (member major-mode drag-stuff-except-modes)
    (drag-stuff-mode +1)))

;;;###autoload
(defun turn-off-drag-stuff-mode ()
  "Turn off `drag-stuff-mode'."
  (interactive)
  (drag-stuff-mode -1))

;;;###autoload
(define-globalized-minor-mode drag-stuff-global-mode
  drag-stuff-mode
  (drag-stuff-mode +1))

;; Dired-mode support

(defsubst dired-current-column ()
  (- (point) (line-beginning-position)))

(defvar drag-stuff-dired-mode-map (make-sparse-keymap)
  "Keymap for `drag-stuff-dired-mode'.")

(defun drag-stuff-dired-up (arg)
  "As `drag-stuff-up' but for Dired (and other read-only) buffers."
  (interactive "p")
  (cond ((dired-get-filename nil t)
         (let ((inhibit-read-only t)
               (drag-stuff--copy-region #'drag-stuff--copy-region)
               (column (dired-current-column)))
           (push-mark (line-beginning-position))
           (goto-char (line-end-position))
           (drag-stuff-line-up arg)
           (goto-char (+ (line-beginning-position) column))))
        (t (message "Not a draggable Dired item"))))

(defun drag-stuff-dired-down (arg)
  "As `drag-stuff-down' but for Dired buffers."
  (interactive "p")
  (cond ((dired-get-filename nil t)
         (let ((inhibit-read-only t)
               (drag-stuff--copy-region #'drag-stuff--copy-region)
               (column (dired-current-column)))
           (push-mark (line-beginning-position))
           (goto-char (line-end-position))
           (drag-stuff-line-down arg)
           (goto-char (+ (line-beginning-position) column))))
        (t (message "Not a draggable Dired item"))))

(defun drag-stuff-define-dired-keys ()
  "Defines keys for `drag-stuff-mode'."
  (let ((map drag-stuff-dired-mode-map))
    (define-key map (drag-stuff--kbd 'up) #'drag-stuff-dired-up)
    (define-key map (drag-stuff--kbd 'down) #'drag-stuff-dired-down)))

;;;###autoload
(define-minor-mode drag-stuff-dired-mode
  "Enable drag-stuff mode in Dired buffers."
  :init-value nil
  :lighter " ddrag")

(provide 'drag-stuff)

;;; drag-stuff.el ends here
