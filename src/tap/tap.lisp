; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun write-tap (o pulses)
  (with-queue q
    (alet (length pulses)
      (format t "Writing TAP file...~%")
      (format o "C64-TAPE-RAW")
      (write-dword 1 o)
      (write-dword ! o)
      (format t "Writing ~A pulses...~%" !)
      (princ pulses o))))
