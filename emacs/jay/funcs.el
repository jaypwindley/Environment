;-----------------------------------------------------------------------
; File:              funcs.el
; Description:       Functions common to all configurations
; Author:            Jay Windley <jwindley>
; Created:           Sat Feb 15 17:05:10 2014
; Copyright:         (c) 2014 Jay Windley
;                    All rights reserved.
;-----------------------------------------------------------------------

(defun getenv-with-default (var default)
  "Dereference an environment variable or use default"
  (interactive "sEnvironment variable: \nsDefault value: \n" )
  (let* ((e (assoc var ENV))
         (v (getenv var)))
    (cond
     (v v)
     (e (let ((val (cdr e)))
	  (cond ((symbolp val) (symbol-name val))
		((listp val) (cdr val))
		((numberp val) (number-to-string val))
		(t val))))
     (t default))))

