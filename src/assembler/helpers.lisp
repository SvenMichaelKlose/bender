; bender – Copyright (c) 2014–2015,2024 Sven Michael Klose <pixel@hugbox.org>

(fn low (x)
  (integer (mod x 256)))

(fn high (x)
  (integer (>> x 8)))

(fn asm (&rest x)
  (| (every #'string? x)
     (assembler-error "ASM expects one or more string."))
  (+@ [parse-string (format nil "~A~%" _)] x))
