; Bender – Copyright (c) 2015 Sven Michael Klose <pixel@hugbox.org>

(defun ascii2petscii (x)
  (? (< #\Z x)
     (- x (-- #\a))
     x))
