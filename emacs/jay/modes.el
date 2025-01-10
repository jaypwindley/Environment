;;;
;;; Custom Mode Assignments
;;;

;; Map some uncommon file suffixes/names to appropriate modes.
;;
(setq auto-mode-alist
      (append '(("\\.tex$"   . latex-mode))
              '(("\\.asm$"   . hlasm-mode))
              '(("\\.h$"     . c++-mode))
              '(("\\.y$"     . c++-mode))
              '(("\\.inl$"   . c++-mode))
              '(("\\.ipp$"   . c++-mode))
              auto-mode-alist))

;;
;; Never use HTML mode.  Ever.
;;
(autoload 'html-mode "html-mode" nil t)

;;
;; Use a better C++ markup
;;
(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)

;;;
;;; Mode hooks
;;;

;;
;; For modes that correspond to software development, add a hook to load
;; the development functions.  F12 will load it if needed in any mode.
;;
(mapc (lambda (mode-hook)
        (add-hook mode-hook (lambda () (load "dev"))))
      '(asm-mode-hook
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
(global-set-key [(f12)] (lambda () (load-file "dev")))

;;
;; Every time a file is loaded, turn on font-lock and fontify the
;; buffer.
;;
(add-hook 'find-file-hooks
          '(lambda ()
             (font-lock-mode t)
             (font-lock-flush)))

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
               (load "hlasm-mode")))))

(add-hook 'sql-mode-hook
          '(lambda()
             (ruler-mode t)))

(add-hook 'protobuf-mode-hook
          '(lambda()
             (cond
              ((string-equal
                (file-name-extension
                 (file-name-nondirectory buffer-file-name)) "proto")
               (load "protobuf-mode")))))

(defun modify-c-style (offset)
  (interactive "sBasic offset: \n")
  (message "C/C++ default style %s" c-default-style)
  (message "C/C++ indent level %d" offset)
  (setq
   comment-column       66
   c-basic-offset       offset
   tab-width            offset
   c-tab-always-indent  nil
   indent-tabs-mode     nil))

(defun namespace-indent-mode ()
  (interactive)
  (let ((code (cdr (assoc 'innamespace c-offsets-alist))))
    (cond ((eq code '+) 'on)
	  (t 'off))))

(defun toggle-namespace-indent()
  (interactive)
  (let* ((prev-state (namespace-indent-mode))
	 (new-state
	  (cond ((eq prev-state 'off)
		 (c-set-offset 'innamespace '+)
		 "on")
		((eq prev-state 'on)
		 (c-set-offset 'innamespace '-)
		 "off"))))
    (message "Namespace indent is %s" new-state)))

;;
;; C and C++ modes get either Kernighan and Ritchie style or Stroustrup style, depending on local
;; context.  Then the style gets local modifications, again according to local context.
;;
(add-hook 'c-mode-hook
          '(lambda ()
             (c-set-style (getenv-with-default "EMACS_C_STYLE" "k&r"))
             (modify-c-style
              (string-to-number (getenv-with-default "EMACS_C_INDENT" "2")))))

(add-hook 'c++-mode-hook
          '(lambda ()
	     (let ((style (getenv-with-default "EMACS_C_STYLE" "stroustrup")))	       
	       (c-set-style (getenv-with-default "EMACS_C_STYLE" "stroustrup"))
	       (message "C++ style %s" style))
             (modify-c-style
              (string-to-number (getenv-with-default "EMACS_C_INDENT" "2")))
	     (global-set-key [(C-f5)] 'toggle-namespace-indent)
	     (modern-c++-font-lock-global-mode t)
             (c-set-offset 'case-label    '+)
             (c-set-offset 'innamespace   '-)
	     (c-set-offset 'inline-open'   0)
             (c-set-offset 'access-label  '/)))


