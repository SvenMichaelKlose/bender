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

(defun pulse2wavlen (x freq cpu-cycles)
  (cl:floor (/ (* x freq) cpu-cycles)))

(defun get-long (in)
  (+ (read-char in)
     (<< (read-char in) 8)
     (<< (read-char in) 16)))

(defun tap2wav (i o freq cpu-cycles)
  (adotimes #x14
    (read-byte i))
  (with-queue q
              (let ocycles 0
    (while (peek-char i)
           nil
      (with (cycles  (+ ocycles
                        (alet (read-byte i)
                          (? (zero? !)
                             (get-long i)
                             (* 8 !))))
             len  (pulse2wavlen cycles freq cpu-cycles)
             scycles  (/ cpu-cycles freq)
             icycles  (* len scycles)
             diff (- cycles icycles)
             corr (/ (* (half diff) 128) scycles))
        (when (< diff 0)
          (error "diff is negative."))
        (when (< 128 corr)
          (error "corr out of range."))
        (= len (half len))
        (alet corr
          (dotimes (i len)
            (enqueue q (- 192 !))
            (= ! 0)))
        (dotimes (i len)
          (enqueue q (+ 64 corr))
          (= corr 0))
        (= ocycles diff)))
    )
    (write-wav o freq 1 8 (queue-list q))))
