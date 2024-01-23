; Bender – Copyright (c) 2015,2024 Sven Michael Klose <pixel@hugbox.org>

(defconstant +vic-defaults-pal+
             '(#x0c #x26 #x16 #x2e #x00 #xc0 #x00 #x00
               #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x1b))

(defconstant +vic-defaults-ntsc+
             '(#x05 #x19 #x16 #x2e #x00 #xc0 #x00 #x00
               #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x1b))

(fn vic-defaults (tv)
  (? (eq tv :ntsc)
     +vic-defaults-ntsc+
     +vic-defaults-pal+))

(fn vic-horigin (tv num-columns)
  (- (elt (vic-defaults tv) 0) (- num-columns 22)))

(fn vic-vorigin (tv num-rows)
  (- (elt (vic-defaults tv) 1) (* 2 (- num-rows 23))))
