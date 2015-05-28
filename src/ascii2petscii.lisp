; Bender – Copyright (c) 2015 Sven Michael Klose <pixel@hugbox.org>

(defun ascii2petscii (x)
  (? (string? x)
     (@ #'ascii2petscii (string-list x))
     (? (< #\Z x)
        (- x (-- #\a))
        x)))
