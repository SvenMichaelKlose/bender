; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun write-wav (o freq channels sample-bits data)
  (format o "RIFF")
  (write-dword (length data) o)
  (format o "WAVEfmt ")
  (write-dword 16 o)
  (write-word #x0001 o)         ; PCM
  (write-word channels o)
  (write-dword freq o)
  (write-dword freq o)
  (write-word 1 o)              ; Block alignment
  (write-word sample-bits o)
  (format o "data")
  (write-dword (length data) o)
  (princ data o))

(defun get-long (in)
  (+ (read-byte in)
     (<< (read-byte in) 8)
     (<< (read-byte in) 16)))

(defun tap2wav (i o freq cpu-cycles &key (sine? nil))
  (= (stream-track-input-location? i) nil)
  (adotimes #x14
    (read-byte i))
  (with-output-file tmp "tap2wav.tmp"
    (with (val 0
           scycles  (/ cpu-cycles freq)
           lcycles scycles
           wr [? (< lcycles 1)
                 (progn
                   (write-byte (integer (+ 128 (/ (+ val (* _ lcycles)) scycles))) tmp)
                   (= val (* _ (- 1 lcycles)))
                   (= lcycles (- scycles (- 1 lcycles))))
                 (progn
                   (+! val _)
                   (--! lcycles))])
      (while (peek-char i)
             nil
        (with (cycles  (integer (alet (read-byte i)
                                  (? (zero? !)
                                     (get-long i)
                                     (* 8 !)))))
          (? sine?
             (adotimes cycles
               (wr (integer (* 64 (degree-sin (* !  (/ 360 cycles)))))))
             (progn
               (dotimes (i (half cycles))
                 (wr 63))
               (dotimes (i (half cycles))
                 (wr -64)))))))
    (alet (fetch-file "tap2wav.tmp")
      (write-wavinfo (make-wavinfo :format-tag 1
                                   :channels 1
                                   :rate freq
                                   :bits 8)
                     (length !)
                     o)
      (princ ! o))))
