(load "funcs")

(linum-mode 1)
(ruler-mode 1)
(set-fill-column 80)
(require 'clang-format)

(defun format-buffer ()
  (if equal major-mode "c++-mode"
      (save-excursion
	(clang-format-region (point-min) (point-max)))))

(add-hook 'before-save-hook
	  (lambda ()
	    (delete-trailing-whitespace)
     	    ; (forrmat-buffer)
	    ))

(defvar jay-date-time "Date and time for source file time stamps.")

;-----------------------------------------------------------------------
; Make a string consisting of the number of spaces it would take to
; center <centered-string> in a column <column-width> characters wide.
;
(defun jay-center-pad-string (centered-string column-width)
  (make-string (/ (- column-width (length centered-string)) 2) ?\ ))


;-----------------------------------------------------------------------
; Make a string consisting of the user's real name and login name in
; the format "Real Name <login>"
;
(defun jay-userid-string ()
  (let* ((realname (getenv-with-default "REALNAME" "Jay Windley"))
	 (logname (getenv-with-default "LOGNAME" "jwindley")))
    (format "%s <%s>" realname logname)))


;-----------------------------------------------------------------------
; Make a string consisting of the copyright symbol, the current year,
; and the name of the company.
;
(defun jay-copyright-string ()
  (interactive)
  (setq jay-date-time (current-time-string))
  (let* ((copyright (getenv-with-default "COPYRIGHT" "Jay Windley")))
    (format "(c) %s %s"
	    (substring jay-date-time (- (length jay-date-time) 4))
	    copyright)))


;-----------------------------------------------------------------------
; Put a comment block at the top of the file, skipping any shell
; specifier which might appear.  The comment block identifies the
; file, its purpose and author, the creation date and time, and
; messages indicating proprietary rights.  <Descr> is a one-line
; comment to briefly identify the purpose of the file and is prompted
; for in the minibuffer.
;
(defun jay-file-header-comment (descr)
  "Insert source file comment."
  (interactive "sOne-line comment: \n")
  (push-mark)
  (goto-char (point-min))
  (if (looking-at "#") (forward-line 1))
  (beginning-of-line)
  (insert-before-markers
comment-start "-----------------------------------------------------------------------" comment-end "\n"
comment-start " File:              " (file-name-nondirectory (buffer-file-name))        comment-end "\n"
comment-start " Description:       " descr comment-end "\n"
comment-start " Author:            " (jay-userid-string) comment-end "\n"
comment-start " Created:           " (current-time-string) comment-end "\n"
comment-start " Copyright:         " (jay-copyright-string) comment-end "\n"
comment-start "                    All rights reserved." comment-end "\n"
comment-start "-----------------------------------------------------------------------" comment-end "\n" ))


;************************************************************
; Put a comment block at the current point as a banner for a
; function.  The function name <fujay-name> is prompted for in
; the minibuffer and centered in the banner.
;
(defun jay-function-long-comment (fujay-name)
  "Insert source file function comment."
  (interactive "sFunction name: \n")
  (beginning-of-line)
  (let* ((pad-string (jay-center-pad-string fujay-name 72)))
    (insert-before-markers
comment-start "-----------------------------------------------------------------------" comment-end "\n"
comment-start pad-string fujay-name pad-string                                          comment-end "\n"
comment-start "-----------------------------------------------------------------------" comment-end "\n"
comment-start "DESCRIPTION"                                                             comment-end "\n"
comment-start "None."                                                                   comment-end "\n"
comment-start " "                                                                       comment-end "\n"
comment-start "ARGUMENTS"                                                               comment-end "\n"
comment-start "None."                                                                   comment-end "\n"
comment-start " "                                                                       comment-end "\n"
comment-start "RETURNS"                                                                 comment-end "\n"
comment-start "None."                                                                   comment-end "\n"
comment-start " "                                                                       comment-end "\n"
comment-start "ASSUMPTIONS"                                                             comment-end "\n"
comment-start "None."                                                                   comment-end "\n"
comment-start " "                                                                       comment-end "\n"
comment-start "SIDE EFFECTS"                                                            comment-end "\n"
comment-start "None."                                                                   comment-end "\n"
comment-start " "                                                                       comment-end "\n"
comment-start "-----------------------------------------------------------------------" comment-end "\n"))
  (forward-line 1))


;-----------------------------------------------------------------------
; Put a comment block at the current point as a banner for an
; identifier or type name.  The identifier name <ident-name> is
; prompted for in the minibuffer and centered in the banner.
;
(defun jay-function-short-comment ()
  "Insert short function comment."
  (interactive)
  (beginning-of-line)
  (insert-before-markers
comment-start "------------------------------------------------------------------------" comment-end "\n"
comment-start "[Insert descriptive comment here]"                                        comment-end "\n"
comment-start                                                                            comment-end "\n")
  (search-backward "[Insert" nil nil 1))


; Keyboard definitions.  This package maps function keys F5-F8 as follows:
;
; KEY  NONE                    CONTROL                      META
; ~~~  ~~~~                    ~~~~~~~                      ~~~~
; F5   align                                                file comments
; F6
; F8   expand abbreviation
; F9   replace string          replace regex

(global-set-key [(f5)]     'align)
(global-set-key [(M-f5)]   'jay-file-header-comment)
(global-set-key [(f8)]     'dabbrev-expand)
(global-set-key [(f9)]     'replace-string)
(global-set-key [(C-f9)]   'replace-regexp)
(global-set-key [?\C-\M-\t] 'format-buffer)
