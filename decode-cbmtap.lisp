; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun decode-cbmtap-byte (o x)
  (when x
    (alet 0
      (dotimes (i 8)
        (= ! (+ (>> ! 1) (? (< x. #x30)
                            0
                            128)))
        (= x ..x))
      (princ (code-char !) o)
      (decode-cbmtap-0 o x))))

(defun decode-cbmtap-0 (o x)
  (when x
    (while (& x (< x. #x50))
           nil
      (= x .x))
    (decode-cbmtap-byte o ..x)))

(defun decode-cbmtap (in-name out-name)
  (with-queue q
    (with-input-file i in-name
      (while (peek-char i)
             nil
        (enqueue q (read-char i))))
    (with-output-file o out-name
      (decode-cbmtap-0 o (queue-list q)))))
