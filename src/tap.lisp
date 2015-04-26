; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun write-tap (o pulses)
  (with-queue q
    (alet (length pulses)
      (format t "Writing TAP file...~%")
      (format o "C64-TAPE-RAW")
      (princ (code-char 1) o)
      (adotimes 3
        (princ (code-char 0) o))
      (princ (code-char (mod ! 256)) o)
      (princ (code-char (mod (>> ! 8) 256)) o)
      (princ (code-char (mod (>> ! 16) 256)) o)
      (princ (code-char (mod (>> ! 24) 256)) o)
      (format t "Writing ~A pulses...~%" !)
      (princ pulses o))))
