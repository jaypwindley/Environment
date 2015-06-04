;-----------------------------------------------------------------------
; File:              colors-dark.el
; Description:       Emacs colors for dark desktop schemes
; Author:            Jay Windley <jwindley>
; Created:           Sat Feb 15 13:49:11 2014
; Copyright:         (c) 2014 Jay Windley
;                    All rights reserved.
;-----------------------------------------------------------------------

;; Active mode line is dark sea green.
(set-face-background 'modeline "#347c8e")

;; Region is slightly lighter than the background.
(set-face-background 'region "#185868")
(set-face-foreground 'region "white")

;;
;;  Default font and buffer attributes
;;
(custom-set-faces
 '(default
    ((t ( :inherit         nil
          :stipple         nil
          :background      "#002B36"                  ; same as Konsole Solar
          :foreground      "gray95"
          :inverse-video   nil
          :box             nil
          :strike-through  nil
          :overline        nil
          :underline       nil
          :slant           normal
          :weight          normal
          :height          80                         ; 8.5 pt
          :width           normal
          :foundry         "unknown"
          :family          "DejaVu Sans Mono" ))))    ; Droid fonts don't work yet

 ;; No markup for built-ins
 ;;
 '(font-lock-builtin-face
   (( ((class              color)
       (min-colors         88)
       (background         dark))
      
     nil) ))

 ;; Comments are light firebrick italics
 ;;
 '(font-lock-comment-face
   (( ((class              color)
       (min-colors         88)
       (background         dark))
      
      ( :foreground        "#ff8888"
        :slant             italic)) ))               

 ;; Functions are bold firebrick
 ;;
 '(font-lock-function-name-face
   (( ((class              color)
       (min-colors         88)
       (background         dark))
      
      ( :foreground        "#ff6666"
        :weight            bold)) ))

 ;; Keywords are simply boldface
 ;;
 '(font-lock-keyword-face
   (( t
      
      ( :weight            bold)) ))

 ;; Strings are wheat colored
 ;;
 '(font-lock-string-face
   (( ((class              color)
       (min-colors         88)
       (background         dark))
      
      ( :foreground        "#ffffcc")) ))

 ;; User-defined types are bright white
 ;;
 '(font-lock-type-face
   (( ((class              color)
       (min-colors         88)
       (background         dark))
      
      ( :foreground        "white")) ))

 ;; Negation characters are emboldened.
 ;;
 '(font-lock-negation-char-face
   (( ((class              color)
       (min-colors         88)
       (background         dark))

      ( :weight            bold)) ))
 
 ;; Need to see this first
 ;;
 '(font-lock-variable-name-face
   (( ((class              color)
       (min-colors         88)
       (background         dark))
      
      ( :foreground        "light cyan")) )))

