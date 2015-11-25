; Bender – Copyright (c) 2015 Sven Michael Klose <pixel@hugbox.org>

(defun ascii2petscii-code (x)
  (? (< #\Z x)
     (- x (-- #\a))
     x))

(defun ascii2petscii (&rest x)
  (? (cons? x.)
     (ascii2petscii (list-string x.)) ; XXX deprecated
     (list-string (@ #'ascii2petscii-code (string-list (apply #'+ x))))))
