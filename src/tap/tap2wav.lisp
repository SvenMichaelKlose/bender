(fn get-long (in)
  (+ (read-byte in)
     (<< (read-byte in) 8)
     (<< (read-byte in) 16)))

(fn pulses2wavdata (i freq cpu-cycles)
  (format t "Converting TAP to WAV…~%")
  (with-output-file tmp "tap2wav.tmp"
    (with (waves    (make-hash-table :test #'==)
           val 0
           scycles  (/ cpu-cycles freq)
           lcycles  scycles
           wr [(? (< lcycles 1)
                  (progn
                    (write-byte (integer (number+ 128 (/ (number+ val (* _ lcycles)) scycles))) tmp)
                    (= val (* _ (number- 1 lcycles)))
                    (= lcycles (- scycles (- 1 lcycles))))
                  (progn
                    (+! val _)
                    (--! lcycles)))
               _])
      (while (peek-char i)
             nil
        (with (cycles  (!= (read-byte i)
                         (? (== 0 !)
                            (get-long i)
                            (* 8 !))))
          (? (< 65535 cycles)
             (adotimes cycles
               (wr 0))
             (!? (href waves cycles)
                 (@ [wr _] !)
                 (with-queue q
                   (adotimes cycles
                     (enqueue q (wr (* 127 (degree-sin (* ! (/ 360 cycles)))))))
                   (= (href waves cycles) (queue-list q)))))))))
  (fetch-file "tap2wav.tmp"))

(fn tap2wav (i o freq cpu-cycles)
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
