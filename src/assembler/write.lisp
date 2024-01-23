(def-instruction write-instruction-operand (instruction out)
  (!= (integer operand)
    (dotimes (i (instruction-operand-size instruction))
      (write-byte (mod ! 256) out)
      (= ! (>> ! 8)))))

(fn write-instruction (instruction out)
  (= *pc* (instruction-address instruction))
  (= *cycles* (instruction-cycles instruction))
  (write-byte (instruction-opcode instruction) out)
  (write-instruction-operand instruction out))

(fn write-assembled-expression (x out)
  (?
    (string? x)
      (prog1 (princ x out)
        (+! *pc* (length x)))
    (| (number? x)
       (character? x))
      (prog1 (write-byte x out)
        (++! *pc*))
    (& (cons? x)
       (| (number? x.)
          (character? x.)))
      (prog1 (adolist x (write-byte ! out))
        (+! *pc* (length x)))
    (instruction? x)
      (write-instruction x out)
    (| (not x)
       (label? x)
       (assignment? x)
       (directive? x))
      nil
    (write-assembled-expressions x out)))

(fn write-assembled-expressions (x out)
  (adolist x
    (with-temporary *assembler-current-line* !
      (adolist (.!)
        (let bytes (with-string-stream sout
                     (write-assembled-expression ! sout))
          (princ bytes out)
          (assemble-dump-line *pc* bytes))))))
