; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(def-instruction write-instruction-operand (instruction out)
  (alet operand
    (dotimes (i (instruction-operand-size instruction))
      (write-byte (mod ! 256) out)
      (= ! (>> ! 8)))))

(defun write-instruction (instruction out)
  (= *pc* (instruction-address instruction))
  (write-byte (instruction-opcode instruction) out)
  (write-instruction-operand instruction out))

(defun write-assembled-expression (x out)
  (?
    (string? x)         (princ x out)
    (number? x)         (write-byte x out)
    (& (cons? x)
       (number? x.))    (adolist x (write-byte ! out))
    (instruction? x)    (write-instruction x out)
    (| (not x)
       (label? x)
       (assignment? x)
       (directive? x))  nil
    (write-assembled-expressions x out)))

(defun write-assembled-expressions (x out)
  (adolist x
    (with-temporary *assembler-current-line* !
      (let pc *pc*
        (adolist (.!)
          (let bytes (with-string-stream sout
                       (write-assembled-expression ! sout))
            (princ bytes out)
            (assemble-dump-line *pc* bytes)))))))
