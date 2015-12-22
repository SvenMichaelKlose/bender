; Bender – Copyright (c) 2015 Sven Michael Klose <pixel@hugbox.org>

(defconstant +vic-defaults-pal+
             '(#x0c #x26 #x16 #x2e #x00 #xc0 #x00 #x00
               #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x1b))

(defconstant +vic-defaults-ntsc+
             '(#x05 #x19 #x16 #x2e #x00 #xc0 #x00 #x00
               #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x1b))

(defun vic-defaults (tv)
  (? (eq tv :ntsc)
     +vic-defaults-ntsc+
     +vic-defaults-pal+))

(defun vic-horigin (tv num-columns)
  (- (elt (vic-defaults tv) 0) (- 22 num-columns)))

(defun vic-vorigin (tv num-rows)
  (- (elt (vic-defaults tv) 0) (* 4 (- 22 num-rows))))
