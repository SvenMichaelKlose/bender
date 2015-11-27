; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defvar *instructions* (make-hash-table :test #'eq))
(defvar *opcode-map* (make-array 256))

(defun make-instruction-by-opcode (x)
  (aprog1 (opcode-instruction x)
    (with (m (instruction-mnemonic !)
           a (instruction-addrmode !))
      (= (aref *opcode-map* x) nil)
      (unless (eq 'ill m)
        (cache (make-hash-table :test #'eq) (href *instructions* m))
        (& (href (href *instructions* m) a)
           (error "Double opcode for ~A." !))
        (= (href (href *instructions* m) a) x)
        (= (aref *opcode-map* x) (. m a))))))

(defun make-instruction-map ()
  (adotimes 256
    (make-instruction-by-opcode !)))

(make-instruction-map)

(defun number-of-legal-opcodes ()
  (length (mapcan [hashkeys (href *instructions* _)]
                  (hashkeys *instructions*))))

(format t "Generated ~A mnemonics and ~A legal opcodes.~%"
        (length (hashkeys *instructions*))
        (number-of-legal-opcodes))

(defun print-instructions (&optional (out *standard-output*))
  (dolist (i (hashkeys *instructions*))
    (format out "~A: ~A~%" i (hashkeys (href *instructions* i)))))

(print-instructions)
