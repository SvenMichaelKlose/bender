; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun assemble-expression (x &key (ensure? nil) (not-zero? nil))
  (| (? (| (number? x)
           (string? x))
        x)
     (case x.
       'expression  (unless (& (not ensure?)
                               (first-pass?))
                      (eval (macroexpand (labels-to-exprs .x))))
       'identifier  (get-label .x :required? (not (first-pass?))))
     (unless not-zero?
       0)))

(defun convert-operand (x)
  (?
    (character? x) (char-code x)
    (string? x)    (? (< 1 (length x))
                      (assembler-error "Operand strings must contain one character.")
                      (char-code (elt x 0)))
    x))

(def-instruction assemble-operand (instruction x)
  (when (eq 'branch addrmode)
    (= x (- x address 2))
    (& (< 2 *pass*)
       (| (< x -128)
          (> x 127))
       (assembler-error "Branch is out of range (~A bytes)." x)))
  (& (eq x 'jmp)
     (<= -128 (- x address 2) 127)
     (assembler-hint "JMP is short enough for a branch."))
  (= (instruction-operand instruction) x))

(def-instruction assemble-instruction (instruction)
  (= (instruction-address instruction) *pc*)
  (& (branch-instruction? mnemonic)
     (= (instruction-addrmode instruction) 'branch))
  (= (instruction-operand instruction) (assemble-operand instruction (assemble-expression operand-expression)))
  (instruction-optimize-addrmode instruction)
  instruction)

(defun assemble-label (x)
  (add-label .x *pc*)
  x)

(defun assemble-assignment (x)
  (add-label .x. (assemble-expression ..x.))
  x)

(defun assemble-identifier (x)
  (| (get-label .x :required? (not (first-pass?)))
     0))

(defun assemble-toplevel-expression (x)
  (!? (assemble-expression x :ensure? t :not-zero? t)
      (? (cons? !)
         (? (number? !.)
            !
            (assemble-parsed-expressions !))
         !)))

(defun update-pc (x pc)
  (+ pc
     (?
       (not x)           0
       (number? x)       1
       (string? x)       (length x)
       (instruction? x)  (instruction-size x)
       (& (cons? x)
          (number? x.))  (length x)
       0)))

(defun catch-end (x)
  (& (cons? x)
     (eq x. 'directive)
     (eq .x. 'end)
     (assemble-end nil)))

(defun assemble (x)
  (? *disabled?*
     (catch-end x)
     (aprog1 (?
               (not x)           x
               (string? x)       x
               (number? x)       x
               (instruction? x)  (assemble-instruction x)
               (case x.
                 'label       (assemble-label x)
                 'assignment  (assemble-assignment x)
                 'directive   (assemble-directive x)
                 'identifier  (assemble-identifier x)
                 'expression  (assemble-toplevel-expression x)
                 (assembler-error "Unexpected parsed expression ~A." x)))
       (= *pc* (update-pc ! *pc*))
       (& *data?*
          (return)))))

(defun assemble-parsed-expressions (x)
  (@ [with-temporary *assembler-current-line* _
       (& (eq *segment-mode* :collect)
          (let-when b *sourceblock-stack*.
            (enqueue (sourceblock-exprs b) _)))
       (. _.
          (? (cons? ._)
             (@ #'assemble ._)
             (assemble ._)))]
     x))
