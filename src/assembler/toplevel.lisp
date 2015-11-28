; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun asm (&rest x)
  (| (every #'string? x)
     (assembler-error "ASM expects one or more string."))
  (mapcan [parse-string (format nil "~A~%" _)] x))

(defun assemble-expression (x &key (ensure? nil) (not-zero? nil))
  (| (& (number? x) x)
     (case x.
       'expression  (unless (& (not ensure?)
                               (first-pass?))
                      (eval (macroexpand (labels-to-exprs .x))))
       'identifier  (get-label .x :required? (not (first-pass?))))
     (unless not-zero?
       0)))

(def-instruction assemble-operand (instruction operand)
  (when (character? operand)
    (= operand (char-code operand)))
  (when (eq 'branch (instruction-addrmode instruction))
    (= operand (- operand *pc* 1))
    (& (< 2 *pass*)
       (| (< operand -128)
          (> operand 127))
       (assembler-error "Branch is out of range (~A bytes)." operand)))
  (& (eq (instruction-mnemonic instruction) 'jmp)
     (<= -128 (- operand *pc* 1) 127)
     (assembler-hint "JMP is short enough for a branch.")))

(def-instruction instruction-write-operand (inst out)
  (dotimes (i (instruction-operand-size inst))
    (write-byte (mod operand 256) out)
    (= operand (>> operand 8))))

(def-instruction instruction-write (instruction out)
  (write-byte (instruction-opcode instruction) out)
  (instruction-write-operand instruction out))

(defun assemble-instruction (instruction)
  (= (instruction-address instruction) *pc*)
  (instruction-optimize-addrmode instruction))

(defun assemble-assignment (x)
  (add-label .x. (assemble-expression ..x.)))

(defun assemble-identifier (x)
  (| (get-label x :required? (not (first-pass?)))
     0))

(defun assemble-toplevel-expression (x)
  (awhen (assemble-expression x :ensure? t :not-zero? t)
    (?
      (cons? !)    (? (number? !.)
                      !
                      (assemble-parsed-expressions !))
      !)))

(defun assemble (x)
  (?
    (not x)           x
    (string? x)       x
    (number? x)       x
    (instruction? x)  (assemble-instruction x)
    (case x.
      'label        (add-label .x *pc*)
      'assignment   (assemble-assignment x)
      'directive    (assemble-directive x)
      'identifier   (assemble-identifier .x)
      'expression   (assemble-toplevel-expression x)
      (assembler-error "Unexpected parsed expression ~A." x))))

(defun assemble-parsed-expressions (x)
  (print x)
  (mapcan [with-temporary *assembler-current-line* _
            (| *assign-blocks-to-segments?*
               (let-when b *sourceblock-stack*.
                 (enqueue (sourceblock-exprs b) _)))
            (? (cons? ._)
               (@ [? *disabled?*
                     (? (& (cons? _)
                           (eq _. 'directive)
                           (eq ._. 'end))
                        (assemble-end nil))
                     (assemble _)]
                  ._)
               (list (assemble ._)))]
          x))

(defun assemble-parsed-files-0 (out-name dump-name x &key (unassigned-segment-blocks nil)
                                                          (segments (make-queue)))
  (clear-labels)
  (= *pass* 0)
  (while (| *label-changed?*
            (< *pass* 4))
         nil
    (format t "Pass ~A…~%" *pass*)
    (= *label-changed?* nil
       *unassigned-segment-blocks* unassigned-segment-blocks
       *segments* segments)
    (= *pc* 0)
    (= *acycles* 0)
    (rewind-labels)
    (print (assemble-parsed-expressions x))
    (++! *pass*))
  (awhen *sourceblock-stack*
    (assembler-error "Block(s) with no END: ~A" !)))

(defun assemble-parsed-files (out-name dump-name x)
  (assemble-parsed-files-0 out-name dump-name x)
  (awhen *unassigned-segment-blocks*
    (format t "Assembling again to assign BLOCKs to SEGMENTS…~%")
    (sort-unassigned-segment-blocks)
    (with-temporary *assign-blocks-to-segments?* t
      (assemble-parsed-files-0 out-name dump-name x :unassigned-segment-blocks !
                                                    :segments *segments*))))

(defun assemble-files (out-name &rest in-names)
  (with-temporary *unassigned-segment-blocks* nil
    (let dump-name (+ out-name ".lst")
      (format t "Assembling to '~A'. Dump file is '~A'…~%"
              out-name dump-name)
      (assemble-parsed-files out-name dump-name (parse-files in-names)))
    (check-on-unassigned-blocks)
    (rewind-labels))
  nil)
