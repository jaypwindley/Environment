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

(let ((class '((class color) (min-colors 89) (background dark)))
      (fg          "gray90" )
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
      (ltlime      "#7affff")
      (redpurple   "#cc79a7")
      (bluegray    "#848ea9"))

  (custom-theme-set-faces
   'jaydark
   `(default
      ((,class
	( :foreground ,fg
	  :background ,bg
	  :slant      normal
	  :weight     normal
	  :height     70
	  :foundry    "unknown"
	  :family     "Droid Sans Mono Dotted for Powerline"))))

   `(cursor ((,class (:background ,vermillion))))

   ;;
   ;; Highlighting faces
   ;;
   `(fringe ((,class ( :background ,bglight))))

   `(highlight ((,class (
	  :foreground  ,blue
	  :background  "#e5e5e5"))))

   `(region ((,class (
	  :foreground unspecified
	  :background ,bglight))))

   `(secondary-selection ((,class (
	  :background "#e5e5e5"))))

   `(isearch ((,class (
	   :underline t
	   :foreground unspecified
	   :background unspecified))))

   `(lazy-highlight ((,class (:foreground ,yellow))))

   `(trailing-whitespace ((,class (:background ,vermillion))))

   ;; Mode line faces
   `(mode-line-inactive ((,class (
	   :background ,bglight
	   :foreground ,ltfirebrick))))

   `(mode-line ((,class (
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
   `(font-lock-builtin-face ((,class (:weight bold :color ,white))))

   `(font-lock-comment-face ((,class (
	  :slant      italic
	  :foreground "gray50"))))

   `(font-lock-comment-delimiter-face ((,class (:foreground "gray35" ))))

   `(font-lock-doc-face ((,class (
	  :slant italic
	  :foreground ,bluegray ))))

   `(font-lock-constant-face ((,class (
	  :weight     bold
	  :foreground ,lime))))

   `(font-lock-function-name-face ((,class (
	  :weight      bold
	  :foreground ,firebrick ))))

   `(font-lock-keyword-face ((,class (
	  :weight     bold
	  :color      ,white ))))

   `(font-lock-string-face ((,class (:foreground ,wheat))))

   `(font-lock-type-face ((,class (
	  :foreground ,white))))

   `(font-lock-variable-name-face ((,class (
	  :foreground ,ltlime))))

   ;; Button and link faces
   `(link         ((,class (:underline t :foreground ,blue))))
   `(link-visited ((,class (:underline t :foreground ,redpurple)))))


;  (custom-theme-set-variables
;   'jaydark
;   `(ansi-color-names-vector ["black" ,vermillion ,bluegreen ,yellow ,ltlime
;			      ,blue ,redpurple "light cyan" ,lime ,skyblue "white"]))
)

(provide-theme 'jaydark)
