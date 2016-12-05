;-----------------------------------------------------------------------
; File:              hlasm-mode.el
; Description:       IBM Assembler H
; Author:            Jay Windley <jwindley>
; Created:           Thu Dec  1 15:31:43 2016
; Copyright:         (c) 2016 Jay Windley
;                    according to terms of the GNU Public License.
;-----------------------------------------------------------------------

;;----------------------------------------------------------------------
;;
;; This mode started out as post-application mods to Eric Raymond's
;; original assembler mode for GNU assembly, in turn based on Martin
;; Neitzel's.  Our original intent was to load asm-mode and then
;; retrospectively modify only what was necessary to get the proper
;; behavior for IBM's baroque assembler.  Turns out "what was necessary"
;; was a lot, mostly due to locally-allocated, literal regular
;; expressions that are quite suitable for GNU but can't be easily
;; overridden.  Hence this is a loose adaptation of esr's original,
;; following its general layout and approach.  The model bears the
;; following copyright notice.
;;
;; Copyright (C) 1991, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
;; Free Software Foundation, Inc.
;; ----------------------------------------------------------------------


(defgroup hlasm nil
  "Mode for editing IBM MVS high-level assembler (HLASM) code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)


;; This is the comment character only when it appears in column 1.
;; Elsewhere it has the meaning of being the current offset such as from
;; the start of a CSECT.  Hence we can't really include it in the syntax
;; table.
;;
(defvar hlasm-comment-char ?\*
  "*The comment-start character in HLASM mode.")

(defvar hlasm-cont-column 71
  "The column where continuation characters go")

(defcustom hlasm-cont-char ?+
  "Character to use as continuation character"
  :type 'character
  :group 'hlasm)

(setq comment-column 40)

(defvar hlasm-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry  ?\n  "> b"  st) ; EOL ends comment
    (modify-syntax-entry  ?\(  "()"   st) ; parens have customary meaning
    (modify-syntax-entry  ?'   "\""   st) ; single-quoted strings
    st)
  "Syntax table used while in HLASM mode.")


;; The typical labeled assembly line looks like
;; 0....|....1....|....2....|....3 ...
;; LABELXXX OPCODE OP1,OP2
;;
;; Only 8 characters of label are significant.  Opcodes and macros are
;; typically no longer than 6 characters.  Operands must not contain
;; bareword whitespace because white space after the operands means any
;; text after the white space through column 71 is considered a comment
;; (no delimiter character required) -- except when column 72 is
;; non-blank, in which case the statement continues on the next line
;; beginning in column 16.
;;
;; Hence the tab strategy looks something like
;; 0....|...T1....|T...2....|....3....|....T(#) ... ...7.T
;; LABELXXX OPCODE OP1,OP2
;;
;; The MVS assembler is a stickler for correct column
;; placement. Therefore disable any attempt to use tabs for spacing,
;; since improperly tabified code will simply gag the assembler.
;;
(setq tab-stop-list '(9 15 comment-column hlasm-continuation-column))
(setq-default indent-tabs-mode nil)


;; The key map.
;;
(defvar hlasm-mode-map
  (let ((map (make-sparse-keymap)))

    ; Keys
    (define-key map ":"		       'hlasm-colon)
    (define-key map "\C-c;"	       'comment-region)
    (define-key map (kbd "<return>")   'newline-and-indent)
    (define-key map (kbd "S-<return>") 'hlasm-continue)

    ; Menu bar
    (define-key map [menu-bar] (make-sparse-keymap))
    (define-key map [menu-bar hlasmasm-mode] (cons "HLASM" map))
    (define-key map [comment-region]
      '(menu-item "Comment Region" comment-region
		  :help "Comment or uncomment each line in the region"))
    map)
  "Keymap for HLASM mode.")


;;
;;
(defconst hlasm-font-lock-keywords
  '(
    ;; Comment starts with the comment character in column 1 and
    ;; continues to the end of the line.
    ("^\*.*$" . font-lock-comment-face)
    
    ;; Odds are there will be some JCL in the file.  Make it look like
    ;; preprocessor directives.
    ("^/[/\*]\\{1\\}.*$" . font-lock-preprocessor-face)

    ;; @todo The preceding two elements should override syntax
    ;; fontification.
    

    ("^\\(\\(\\sw\\|\\s_\\)+\\)\\>[ \t]*\\(\\sw+\\(\\.\\sw+\\)*\\)?"
     (1 font-lock-function-name-face)
     (3 font-lock-keyword-face nil t))
    
    ;; Opcode mnemonics
    ("^\\((\\sw+)\\)?\\s +\\(\\(\\.?\\sw\\|\\s_\\)+\\(\\.\\sw+\\)*\\)"
     2 font-lock-keyword-face)

    ;; Registers are upper-case R followed by 1 or 2 digits.
    ("R[[:digit:]]\\{1,2\\}" . font-lock-variable-name-face))
  "Additional expressions to highlight in Assembler mode.")


(defun hlasm-mode ()
  "Major mode for editing IBM high-level assembler code."  
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "IBM HLASM")
  (setq major-mode 'hlasm-mode)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(hlasm-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'hlasm-indent-line)

  (set (make-local-variable 'tab-always-indent) nil)

  (use-local-map (nconc (make-sparse-keymap) hlasm-mode-map))

  (set-syntax-table (make-syntax-table hlasm-mode-syntax-table))

  (make-local-variable 'comment-start)
  (setq comment-start (string hlasm-comment-char))
  (make-local-variable 'comment-add)
  (setq comment-add 1)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\(?:\\s<+\\|/[/*]+\\)[ \t]*")
  (make-local-variable 'comment-end-skip)
  (setq comment-end-skip "[ \t]*\\(\\s>\\|\\*+/\\)")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (setq fill-prefix "\t")
  (run-mode-hooks 'hlasm-mode-hook))

(defun hlasm-indent-line ()
  "Auto-indent the current line."
  (interactive)
  (let* ((savep (point))
	 (indent (condition-case nil
		     (save-excursion
		       (forward-line 0)
		       (skip-chars-forward " \t")
		       (if (>= (point) savep) (setq savep nil))
		       (max (asm-calculate-indentation) 0))
		   (error 0))))
    (if savep
	(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun hlasm-calculate-indentation ()
  (or
   ;; Flush labels to the left margin.
   (and (looking-at "\\(\\sw\\|\\s_\\)+:") 0)
   ;; Same thing for `;;;' comments.
   (and (looking-at "\\s<\\s<\\s<") 0)
   ;; Simple `;' comments go to the comment-column.
   (and (looking-at "\\s<\\(\\S<\\|\\'\\)") comment-column)
   ;; The rest goes at the first tab stop.
   (or (car tab-stop-list) tab-width)))


(defun hlasm-colon ()
  "Insert a colon; if it follows a label, delete the label's indentation."
  (interactive)
  (let ((labelp nil))
    (save-excursion
      (skip-syntax-backward "w_")
      (skip-syntax-backward " ")
      (if (setq labelp (bolp)) (delete-horizontal-space)))
    (call-interactively 'self-insert-command)
    (when labelp
      (delete-horizontal-space)
      (tab-to-tab-stop))))

(defun hlasm-continue ()
  "Add continuation character, newline and indent to continuation column"
  (interactive)
  (move-to-column hlasm-cont-column t)
  (insert hlasm-cont-char)
  (newline)
  (tab-to-tab-stop) (tab-to-tab-stop))


(defun hlasm-comment ()
  "Convert an empty comment to a `larger' kind, or start a new one.
These are the known comment classes:
   1 -- comment to the right of the code (at the comment-column)
   2 -- comment on its own line, indented like code
   3 -- comment on its own line, beginning at the left-most column.
Suggested usage:  while writing your code, trigger asm-comment
repeatedly until you are satisfied with the kind of comment."
  (interactive)
  (comment-normalize-vars)
  (let (comempty comment)
    (save-excursion
      (beginning-of-line)
      (with-no-warnings
	(setq comment (comment-search-forward (line-end-position) t)))
      (setq comempty (looking-at "[ \t]*$")))

  (cond

   ;; Blank line?  Then start comment at code indent level.
   ;; Just like `comment-dwim'.  -stef
   ((save-excursion (beginning-of-line) (looking-at "^[ \t]*$"))
    (indent-according-to-mode)
    (insert hlasm-comment-char ?\ ))

   ;; Nonblank line w/o comment => start a comment at comment-column.
   ;; Also: point before the comment => jump inside.
   ((or (null comment) (< (point) comment))
    (indent-for-comment))

   ;; Flush-left or non-empty comment present => just insert character.
   ((or (not comempty) (save-excursion (goto-char comment) (bolp)))
    (insert hlasm-comment-char))

   ;; Empty code-level comment => upgrade to next comment level.
   ((save-excursion (goto-char comment) (skip-chars-backward " \t") (bolp))
    (goto-char comment)
    (insert hlasm-comment-char)
    (indent-for-comment))

   ;; Empty comment ends non-empty code line => new comment above.
   (t
    (goto-char comment)
    (skip-chars-backward " \t")
    (delete-region (point) (line-end-position))
    (beginning-of-line) (insert "\n") (backward-char)
    (hlasm-comment)))))

(provide 'hlasm-mode)
