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
  (/ (* 8 freq x) cpu-cycles))

(defun longpulse2wavlen (in freq cpu-cycles)
  (alet (+ (read-char in)
           (<< (read-char in) 8)
           (<< (read-char in) 16))
    (/ (* freq !) cpu-cycles)))

(defun tap2wav (i o freq cpu-cycles)
  (adotimes #x14
    (read-byte i))
  (with-queue q
    (while (peek-char i)
           nil
      (alet (half (alet (read-byte i)
                    (? (zero? !)
                       (longpulse2wavlen i freq cpu-cycles)
                       (pulse2wavlen ! freq cpu-cycles))))
        (dotimes (i !)
          (enqueue q 0))
        (dotimes (i !)
          (enqueue q 255))))
    (write-wav o freq 1 8 (queue-list q))))
