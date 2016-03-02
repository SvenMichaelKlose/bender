; bender – Copyright (c) 2014–2016 Sven Michael Klose <pixel@hugbox.org>

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

(defun pulses2wavdata (i freq cpu-cycles)
  (with-output-file tmp "tap2wav.tmp"
    (with (val 0
           scycles  (/ cpu-cycles freq)
           lcycles scycles
           wr [? (< lcycles 1)
                 (progn
                   (write-byte (integer (number+ 128 (/ (number+ val (* _ lcycles)) scycles))) tmp)
                   (= val (* _ (number- 1 lcycles)))
                   (= lcycles (- scycles (- 1 lcycles))))
                 (progn
                   (+! val _)
                   (--! lcycles))])
      (while (peek-char i)
             nil
        (with (cycles  (alet (read-byte i)
                         (? (zero? !)
                            (get-long i)
                            (* 8 !))))
          (? (< 65535 cycles)
             (adotimes cycles
               (wr 0))
             (adotimes cycles
               (wr (* 64 (degree-sin (* !  (/ 360 cycles)))))))))))
  (fetch-file "tap2wav.tmp"))

(defun tap2wav (i o freq cpu-cycles)
  (= (stream-track-input-location? i) nil)
  (adotimes #x14
    (read-byte i))
  (alet (pulses2wavdata i freq cpu-cycles)
    (format t "Writing generated WAV…~%")
    (write-wavinfo (make-wavinfo :format-tag 1
                                 :channels 1
                                 :rate freq
                                 :bits 8)
                   (length !)
                   o)
    (princ ! o)))
