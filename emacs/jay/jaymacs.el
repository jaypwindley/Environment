;-----------------------------------------------------------------------
; File:              jaymacs.el
; Description:       GNU Emacs startup code
; Author:            Jay Windley <jwindley>
; Created:           Aug 27 13:58:50 1996
; Copyright:         (c) 1996-2014 Jay Windley
;                    All rights reserved.
;-----------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/jay")
(load "funcs")

;;; Load backup environment, if present.  Not all methods of invoking Emacs read the environment as
;;; set up by bash.
(let ((env-path "~/.emacs.d/env.el"))
  (cond ((file-exists-p env-path) (load env-path))
	(t (defvar ENV nil))))
(message "Local Emacs Lisp environment %s" ENV)

;;-----------------------------------------------------------------------------
;; Colors and fonts
;;-----------------------------------------------------------------------------

;;
;; Even ASCII terminals can often make some use of font-lock, so always
;; load it.
;;
(require 'font-lock)                    ; Use Emacs font-lock
(setq font-lock-maximum-decoration 3)   ; ...and use all of it

;;
;; Look for a hint about the desktop color scheme and load the font-lock
;; definitions that best fit.  For ASCII terminals use a font-lock
;;"scheme" composed only of different weights.
;;
(let (scheme (getenv "DESKTOP_COLOR_SCHEME"))
   (cond ((eq emacs-major-version 23)
	 (cond ((eq scheme "dark")
		(load "colors-dark"))
	       ((eq scheme "light")
		(load "colors-light"))
	       (t (cond (window-system (load "colors-dark"))
			(t (load "colors-term"))))))
	((>= emacs-major-version 24)
	 (add-to-list 'custom-theme-load-path "~/.emacs.d/jay")
	 (load-theme 'wombat t)
	 (load-theme 'jaydark t))))

;;
;; Set transparency
;;
(let ((active-alpha 80)
       (inactive-alpha 65))
  (set-frame-parameter (selected-frame) 'alpha
		       (list active-alpha inactive-alpha))
  (add-to-list 'default-frame-alist
	       (list 'alpha active-alpha inactive-alpha)))






;;-----------------------------------------------------------------------------
;; Global Behavior
;;-----------------------------------------------------------------------------

(cond (window-system
       (mwheel-install)
       (set-frame-size (selected-frame) 120 69)
       (add-hook 'after-make-frame-functions
		 '(lambda (frame)
		    (set-frame-size frame 120 69)))))

; @todo Figure out how do to this with lexical scoping.

(put 'upcase-region 'disabled nil)      ; allow silent case conversions
(ruler-mode)                            ; horizontal ruler

;;
;;  Customization variables.
;;
;;  If you use the built-in customization wizard, the variables it
;;  sets will be added to the global init file and must be copied here
;;  in order to take effect.  This is a feature.
;;
(custom-set-variables
 '(default-major-mode              'text-mode        )  ; default major mode
 '(require-final-newline           nil               )  ; files don't always end with newline, e.g., X12
 '(explicit-shell-file-name        "bash"            )  ; execute shell commands with this
 '(c-tab-always-indent             t                 )  ; internal TABs are really ASCII TAB
 '(next-line-add-newlines          nil               )  ; down-arrow at EOF doesn't add lines
 '(line-number-mode                t                 )  ; put the line number on the mode line
 '(column-number-mode              t                 )  ; put the column number on the mode line
 '(fill-column                     72                )  ; wrap at column 72
 '(delete-selection-mode           t                 )  ; keypress replaces selection
 '(transient-mark-mode             t                 )  ; can select with mouse
 '(inhibit-startup-screen          t                 )  ; suppress copyright notice crap
 '(case-fold-search                t                 )  ; searches wrap to top
 '(current-language-environment    "Latin-9"         )  ; speak English
 '(default-input-method            "latin-9-prefix"  )  ;
 '(global-font-lock-mode           t nil (font-lock) )  ; use only true font-lock
 '(tool-bar-mode                   nil               )  ; no toolbar
			                                            ; alternate fonts for font-lock
 '(face-font-family-alternatives   (quote (("lucidatypewriter" "courier" "fixed")
			    ("lucidatypewriter" "helv" "helvetica" "arial" "fixed"))))
 )

;;
;; Global key bindings
;;
(global-set-key [delete]           'delete-char)
(global-set-key [kp-delete]        'delete-char)
(global-set-key "\010"             'backward-delete-char-untabify)   ; Backspace works
(global-set-key "\015"             'newline-and-indent)              ; Return indents
(global-set-key [C-tab]            (lambda () (interactive) (insert-char 9 1)))
(global-set-key "\C-xg"            'goto-line)
(global-set-key [(f1)]             'save-buffer)
(global-set-key [(C-f1)]           (lambda () (interactive) (save-buffer) (kill-emacs)))
(global-set-key [(f2)]             'undo)
(global-set-key [(f3)]             'find-file-other-window)
(global-set-key [(C-f3)]           (lambda () (interactive) (kill-buffer nil)))
(global-set-key [(f4)]             'set-mark-command)
(global-set-key [mouse-8]          'beginning-of-buffer)
(global-set-key [mouse-10]         'end-of-buffer)


;;
;; Set up mode-specific things.
;;
(load "modes")
