; bender – Copyright (c) 2014–2015,2024 Sven Michael Klose <pixel@copei.de>

(var *assembler-dump-stream* nil)

(fn print-dump-header (o)
  (format o ";~%")
  (format o "; Pass ~A~%" *pass*)
  (format o ";~%")
  (format o "; Adress | Cycles | Accumulated cycles | Bytes | Source~%"))

(fn assemble-dump-line (pc bytes)
  (let o *assembler-dump-stream*
    (fresh-line o)
    (print-hexword pc o)
    (princ ":" o)
    (format o " ~A ~A :" (| *cycles* " ") *acycles*)
    (& *cycles* (+! *acycles* *cycles*))
    (adolist ((string-list bytes))
      (princ " " o)
      (print-hexbyte (char-code !) o))
    (while (< (stream-location-column (stream-output-location o)) 26)
           nil
       (princ " " o))
    (princ *assembler-current-line*.. o)))
