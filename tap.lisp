;;;;; bender – Copyright (c) 2014 Sven Michael Klose <pixel@copei.de>

(defun write-tap (o pulses)
  (with-queue q
    (with (size    (length pulses))
      (format t "Writing TAP file...~%")
      (format o "C64-TAPE-RAW")
      (princ (code-char 1) o)
      (adotimes 3
        (princ (code-char 0) o))
      (princ (code-char (mod size 256)) o)
      (princ (code-char (mod (>> size 8) 256)) o)
      (princ (code-char (mod (>> size 16) 256)) o)
      (princ (code-char (mod (>> size 24) 256)) o)
      (format t "Writing ~A pulses...~%" size)
      (princ pulses o))))
