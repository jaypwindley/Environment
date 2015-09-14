;-----------------------------------------------------------------------
; File:              jaydark-theme.el
; Description:       Newfangled theme for dark colors
; Author:            Jay Windley <jwindley>
; Created:           Tue Mar  3 23:27:56 2015
; Copyright:         (c) 2015 Jay Windley
;                    All rights reserved.
;-----------------------------------------------------------------------

(deftheme jaydark
  "Jay's theme for dark color schemes.")

(let ((class '((class color) (min-colors 89)))
      (bg          "#2F4047")
      (bglight     "#003B46")
      (ltfirebrick "#ff8888")
      (firebrick   "#ff6666")
      (wheat       "#ffffcc")
      (white       "#ffffff")
      
      (orange      "#e69f00")
      (skyblue     "#56b4e9")
      (bluegreen   "#009e73")
      (yellow      "#f8ec59")
      (blue        "#0072b2")
      (vermillion  "#d55e00")
      (lime        "#008080")
      (redpurple   "#cc79a7")
      (bluegray    "#848ea9"))
  
  (custom-theme-set-faces
   'jaydark
   `(default
      ((,class
	( :foreground "gray95"
	  :background ,bg
	  :slant      normal
	  :weight     normal
	  :height     80
	  :foundry    "unknown"
	  :family     "DejaVu Sans Mono"))))
   
   `(cursor ((,class (
	  :background ,vermillion))))

   ;;
   ;; Highlighting faces
   ;;
   `(fringe ((,class (
	  :background  ,bglight))))
   
   `(highlight ((,class (
	  :foreground  ,blue
	  :background  "#e5e5e5"))))

   `(region ((,class (
	  :foreground unspecified
	  :background ,bglight))))

   `(secondary-selection ((,class (
	  :background "#e5e5e5"))))

   `(isearch ((,class (
	  :foreground "white"
	  :background ,vermillion))))

   `(lazy-highlight ((,class (
	  :foreground "white"
	  :background ,redpurple))))

   `(trailing-whitespace ((,class (
	  :background ,vermillion))))
   
   ;; Mode line faces
   `(mode-line ((,class (
	   :box ( :line-width   -1
		  :style        released-button )
	   :background ,bglight
	   :foreground ,ltfirebrick))))

   `(mode-line-inactive ((,class (
	   :box ( :line-width  -1
		  :style       released-button)
	   :background "#b0b0b0"
	   :foreground "black"))))

   ;;
   ;; Escape and prompt faces
   ;;
   `(minibuffer-prompt ((,class (
	  :weight bold
	  :foreground  ,blue))))

   `(escape-glyph ((,class (
	  :foreground  ,vermillion))))

   `(error ((,class (
	  :weight      bold
	  :slant       italic
	  :foreground  ,vermillion))))

   `(warning ((,class (
	  :foreground ,orange))))

   `(success ((,class (
	  :foreground ,bluegreen))))

   ;;
   ;; Font lock faces
   ;;
   `(font-lock-comment-face ((,class (
	  :slant      italic
	  :foreground ,ltfirebrick))))
   
   `(font-lock-constant-face ((,class (
	  :weight     bold
	  :foreground ,lime))))
   
   `(font-lock-function-name-face ((,class (
	  :foreground ,firebrick
	  :weight     bold))))
   
   `(font-lock-keyword-face ((,class (
	  :weight     bold ))))
   
   `(font-lock-string-face ((,class (
	  :foreground ,wheat))))
   
   `(font-lock-type-face ((,class (
	  :foreground ,white))))
   
   `(font-lock-variable-name-face ((,class (
	  :foreground "light cyan"))))

   ;; Button and link faces
   `(link         ((,class (:underline t :foreground ,blue))))
   `(link-visited ((,class (:underline t :foreground ,redpurple)))))


  (custom-theme-set-variables
   'jaydark
   `(ansi-color-names-vector ["black" ,vermillion ,bluegreen ,yellow
			      ,blue ,redpurple "light cyan" ,lime ,skyblue "white"])))

(provide-theme 'jaydark)


