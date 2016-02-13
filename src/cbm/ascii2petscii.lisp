; Bender – Copyright (c) 2015–2016 Sven Michael Klose <pixel@hugbox.org>

(defun ascii2petscii-code (x)
  (? (character< #\Z x)
     (character- x #\a (code-char 1))
     x))

(defun ascii2petscii (&rest x)
  (? (cons? x.)
     (ascii2petscii (list-string x.)) ; XXX deprecated
     (list-string (@ #'ascii2petscii-code (string-list (apply #'+ x))))))
