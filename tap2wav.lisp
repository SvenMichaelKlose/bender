; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun print-byte (x o)
  (princ (code-char x) o))

(defun print-word (x o)
  (print-byte (mod x 256) o)
  (print-byte (mod (>> x 8) 256) o))

(defun print-dword (x o)
  (print-word (mod x 65536) o)
  (print-word (mod (>> x 16) 65536) o))

(defun write-wav (o freq channels bitrate data)
  (format o "RIFF")
  (print-dword (length data) o)
  (format o "WAVEfmt ")
  (print-dword 16 o)
  (print-word #x0001 o)         ; PCM
  (print-word channels o)
  (print-dword freq o)
  (print-dword freq o)
  (print-word 1 o)              ; Block alignment
  (print-word bitrate o)
  (format o "data")
  (print-dword (length data) o)
  (adolist data
    (princ (code-char !) o)))

(defun pulse2wavlen (x)
  (/ (* 8 48000 x) 1108405))

(defun longpulse2wavlen (in)
  (alet (+ (read-char in)
           (<< (read-char in) 8)
           (<< (read-char in) 16))
    (/ (* 48000 !) 1108405)))

(defun tap2wav (i o)
  (adotimes #x14
    (read-char i))
  (with-queue q
    (while (peek-char i)
           nil
      (alet (half (alet (read-char i)
                    (? (zero? !)
                       (longpulse2wavlen i)
                       (pulse2wavlen !))))
        (dotimes (i !)
          (enqueue q 0))
        (dotimes (i !)
          (enqueue q 255))))
    (write-wav o 48000 1 8 (queue-list q))))
