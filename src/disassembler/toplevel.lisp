; bender – Copyright (c) 2014 Sven Michael Klose <pixel@copei.de>

(defun disassemble-file (in-name out-name)
  (with-input-file in in-name
    (with-output-file out out-name
      (let pc 0
        (awhile (stream-instruction in)
                nil
          (princ "$" out)
          (print-hexword pc out)
          (print-instruction ! pc out)
          (= pc (+ pc (instruction-size !))))))))
