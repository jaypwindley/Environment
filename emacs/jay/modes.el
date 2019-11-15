;-----------------------------------------------------------------------
; File:              modes.el
; Description:       Manage Emacs modes
; Author:            Jay Windley <jwindley>
; Created:           Sat Feb 15 17:27:15 2014
; Copyright:         (c) 2014 Jay Windley
;                    All rights reserved.
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;                        Custom Mode Assignments
;-----------------------------------------------------------------------
;; Map some uncommon file suffixes/names to appropriate modes.
;;
(setq auto-mode-alist
      (append '(("\\.tex$"   . latex-mode))
              '(("\\.asm$"   . hlasm-mode))
              '(("\\.h$"     . c++-mode))
              '(("\\.y$"     . c++-mode))
              '(("\\.inl$"   . c++-mode))
              '(("\\.proto$" . protobuf-mode))
              auto-mode-alist))

;;
;; Never use HTML mode.  Ever.
;;
(autoload 'html-mode "html-mode" nil t)


;-----------------------------------------------------------------------
;                        Mode Hooks
;-----------------------------------------------------------------------

;;
;; For modes that correspond to software development, add a hook to load
;; the development functions.  F12 will load it if needed in any mode.
;;
(mapc (lambda (mode-hook)
	(add-hook mode-hook (lambda () (load "~/.emacs.d/jay/dev"))))
      '( asm-mode-hook
	 c-mode-hook
	 c++-mode-hook
	 fortran-mode-hook
	 hlasm-mode-hook
	 python-mode-hook
	 perl-mode-hook
	 protobuf-mode-hook
	 sh-mode-hook
	 sql-mode-hook
	 bash-mode-hook
	 emacs-lisp-mode-hook
	 ))
(global-set-key [(f12)] (lambda () (load-file "~/.emacs.d/jay/dev")))

;;
;; Every time a file is loaded, turn on font-lock and fontify the
;; buffer.
;;
(add-hook 'find-file-hooks
	  '(lambda ()
	     (font-lock-mode t)
	     (font-lock-fontify-buffer)))

;;
;; Rulers and fill mode in text.
;;
(add-hook 'text-mode-hook
	  '(lambda ()
	     (ruler-mode t)
	     (turn-on-auto-fill)
	     (text-mode-hook-identify)))

;;
;; Assembly mode gets modified behavior if it's editing IBM HLASM code,
;; which we arbitrarily decide will have the .asm suffix.  The .s suffix
;; invokes Eric Raymond's original assembly mode for GNU-type assemblers.
;;
(add-hook 'asm-mode-hook
	  '(lambda()
	     (cond
	      ((string-equal
		(file-name-extension
		 (file-name-nondirectory buffer-file-name)) "asm")
	       (load "~/.emacs.d/jay/hlasm-mode")))))

(add-hook 'sql-mode-hook
	  '(lambda()
	     (ruler-mode t)))

(add-hook 'protobuf-mode-hook
	  '(lambda()
	     (cond
	      ((string-equal
		(file-name-extension
		 (file-name-nondirectory buffer-file-name)) "proto")
	       (load "~/.emacs.d/jay/protobuf-mode")))))

;;
;; C and C++ modes get either Kernighan and Ritchie style or Stroustrup
;; style, depending on context, plus tab stops of 4.
;;
(add-hook 'c-mode-hook
	  '(lambda ()
	     (c-set-style (getenv-with-default "EMACS_C_STYLE" "K&R"))
	     (setq c-basic-offset 4)
	     (setq c-tab-always-indent nil)
	     (setq tab-width 4)
	     (setq indent-tabs-mode nil)))

(add-hook 'c++-mode-hook
	  '(lambda ()
	     (c-set-style (getenv-with-default "EMACS_C_STYLE" "K&R"))
	     (setq-default tab-width 4)
	     (setq-default c-tab-always-indent nil)
	     (setq-default indent-tabs-mode nil)
	     (setq         c-basic-offset 2)))
