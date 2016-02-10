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
  (adolist data
    (write-byte ! o)))

(defun get-long (in)
  (+ (read-char in)
     (<< (read-char in) 8)
     (<< (read-char in) 16)))

(defun tap2wav (i o freq cpu-cycles)
  (adotimes #x14
    (read-byte i))
  (with-queue q
    (with (val 0
           scycles  (/ cpu-cycles freq)
           lcycles scycles
           wr [
               (? (< lcycles 1)
                 (progn
                   (enqueue q (+ 128 (/ (+ val (* _ lcycles)) scycles)))
                   (= val (* _ (- 1 lcycles)))
                   (= lcycles (- scycles (- 1 lcycles))))
                 (progn
                   (+! val _)
                   (--! lcycles)))]
           )
      (while (peek-char i)
             nil
        (with (cycles  (alet (read-byte i)
                         (? (zero? !)
                            (get-long i)
                            (* 8 !))))
          (adotimes cycles
            (wr (* 64 (degree-sin (* ! (/ 360 cycles)))))))))
;          (dotimes (i (half cycles))
;            (wr 63))
;          (dotimes (i (half cycles))
;            (wr -64)))))
    (write-wav o freq 1 8 (queue-list q))))
