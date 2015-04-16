;;;;; bender – Copyright (c) 2014 Sven Michael Klose <pixel@copei.de>

(defun low (x)
  (integer (mod x 256)))

(defun high (x)
  (integer (>> x 8)))
