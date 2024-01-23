; bender – Copyright (c) 2014–2015,2024 Sven Michael Klose <pixel@copei.de>

(fn write-converted-tap (o pulses original-cycles converted-cycles &key (fine? nil))
  (dotimes (i (length pulses))
    (!= (integer (/ (* (elt pulses i) 8 converted-cycles) original-cycles))
      (? fine?
         (progn
           (write-byte 0 o)
           (write-byte (bit-and ! 255) o)
           (write-byte (bit-and (>> ! 8) 255) o)
           (write-byte (bit-and (>> ! 16) 255) o))
         (write-byte (/ ! 8) o)))))

(fn write-tap (o pulses &key (original-cycles nil) (converted-cycles nil))
  (!= (length pulses)
    (format t "Writing TAP file of ~A pulses…~%" !)
    (format o "C64-TAPE-RAW")
    (write-dword 1 o)
    (write-dword ! o)
    (? converted-cycles
       (write-converted-tap o pulses original-cycles converted-cycles)
       (princ pulses o))))
