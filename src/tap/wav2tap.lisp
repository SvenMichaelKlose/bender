; bender – Copyright (c) 2016,2024 Sven Michael Klose <pixel@hugbox.org>

(fn wav2tap-0 (i o cpu-cycles)
  (!= (read-wavinfo i)
    (| (== 1 (wavinfo-format-tag !))
       (error "WAV file data isn't in PCM format."))
    (| (== 1 (wavinfo-channels !))
       (error "WAV file data isn't mono."))
    (| (== 16 (wavinfo-bits !))
       (error "WAV file data in in 16-bit format."))
    (with (last-middle  0
           last-value   0
           last-high    #x8001
           last-low     #x7fff
           middle       0
           sample-rate  (wavinfo-sample-rate !)
           min-length   (/ sample-rate 8000))
      (awhile (read-word i)
              nil
        (= ! (bit-xor ! #x8000))
        (when (< last-high !)
          (= last-high !))
        (when (< ! last-low)
          (= last-low !))
        (= middle (+ last-low (half (- last-high last-low))))
        (when (& (< min-length last-middle)
                 (< (+ middle #x800) last-high)
                 (< middle last-value)
                 (< ! middle))
          (let cycles (integer (* last-middle (/ cpu-cycles sample-rate)))
            (? (< cycles #x800)
               (write-byte (/ cycles 8) o)
               (write-dword (<< cycles 8) o))
            (= last-middle 0)
            (= last-high #x8001)
            (= last-low #x7fff)))
        (++! last-middle)
        (= last-value !)))))

(fn wav2tap (i o &key cpu-cycles)
  (write-tap o (with-string-stream pulses
                 (wav2tap-0 i pulses cpu-cycles))))
