; bender – Copyright (c) 2014–2015,2024 Sven Michael Klose <pixel@copei.de>

(var *instructions* (make-hash-table :test #'eq))
(var *opcode-map* (make-array 256))

(fn make-instruction-by-opcode (x)
  (aprog1 (opcode-instruction x)
    (with (m (instruction-mnemonic !)
           a (instruction-addrmode !))
      (= (aref *opcode-map* x) nil)
      (unless (eq 'ill m)
        (cache (href *instructions* m)
               (make-hash-table :test #'eq))
        (& (href (href *instructions* m) a)
           (error "Double opcode for ~A." !))
        (= (href (href *instructions* m) a) x)
        (= (aref *opcode-map* x) (. m a))))))

(fn make-instruction-map ()
  (adotimes 256
    (make-instruction-by-opcode !)))

(make-instruction-map)

(fn number-of-legal-opcodes ()
  (length (+@ [hashkeys (href *instructions* _)]
              (hashkeys *instructions*))))

(format t "Generated ~A mnemonics and ~A legal opcodes.~%"
        (length (hashkeys *instructions*))
        (number-of-legal-opcodes))

(fn print-instructions (&optional (out *standard-output*))
  (@ (i (hashkeys *instructions*))
    (format out "~A: ~A~%" i (hashkeys (href *instructions* i)))))

(print-instructions)
