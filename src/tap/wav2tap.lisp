; bender – Copyright (c) 2016 Sven Michael Klose <pixel@hugbox.org>

(defun wav2tap-0 (i o cpu-cycles)
  (alet (read-wavinfo i)
    (| (== 1 (wavinfo-format-tag !))
       (error "WAV file data isn't in PCM format."))
    (| (== 1 (wavinfo-channels !))
       (error "WAV file data isn't mono."))
    (| (== 16 (wavinfo-bits !))
       (error "WAV file data in in 16-bit format."))
    (with (last-low     0
           last-value   0
           sample-rate  (wavinfo-sample-rate !))
      (awhile (read-word i)
              nil
        (= ! (bit-xor ! #x8000))
        (when (& (< #x8000 last-value)
                 (< ! #x8000))
          (let cycles (integer (* last-low (/ cpu-cycles sample-rate)))
            (? (< cycles #x800)
               (write-byte (/ cycles 8) o)
               (write-dword (<< cycles 8) o))
            (= last-low 0)))
        (++! last-low)
        (= last-value !)))))

(defun wav2tap (i o &key cpu-cycles)
  (write-tap o (with-string-stream pulses
                 (wav2tap-0 i pulses cpu-cycles))))
