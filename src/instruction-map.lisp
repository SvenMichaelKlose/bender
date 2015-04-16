;;;;; bender – Copyright (c) 2014 Sven Michael Klose <pixel@copei.de>

(defvar *instructions* (make-hash-table :test #'eq))

(defun make-instruction-by-opcode (x)
  (aprog1 (opcode-instruction x)
    (let m (instruction-mnemonic !)
      (unless (eq 'ill m)
        (cache (make-hash-table :test #'eq) (href *instructions* m))
        (& (href (href *instructions* m) (instruction-addrmode !))
           (error "Double opcode for ~A." !))
        (= (href (href *instructions* m) (instruction-addrmode !)) x)))))

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

(defun print-instruction-addressing-modes (&optional (out *standard-output*))
  (dolist (i (hashkeys *instructions*))
    (format out "~A: ~A~%" i (hashkeys (href *instructions* i)))))

(defun generate-opcode (mnemonic addrmode)
  (alet (href *instructions* mnemonic)
    (| (href ! addrmode)
       (? (in? addrmode 'abs 'zp)
          (href ! 'branch))
       (error "Incorrect addressing mode ~A for ~A." addrmode mnemonic))))
