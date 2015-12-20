; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@hugbox.org>

(defun low (x)
  (integer (mod x 256)))

(defun high (x)
  (integer (>> x 8)))

(defun asm (&rest x)
  (| (every #'string? x)
     (assembler-error "ASM expects one or more string."))
  (mapcan [parse-string (format nil "~A~%" _)] x))
