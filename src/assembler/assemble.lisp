(fn assemble-expression (x &key (ensure? nil) (not-zero? nil))
  (| (? (| (number? x)
           (character? x)
           (string? x))
        x)
     (case x.
       'expression  (unless (& (not ensure?)
                               (first-pass?))
                      (eval (macroexpand (labels-to-exprs .x))))
       'identifier  (get-label .x :required? (not (first-pass?))))
     (unless not-zero?
       0)))

(fn convert-operand (x)
  (?
    (character? x)
      (char-code x)
    (string? x)
      (? (< 1 (length x))
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
  (= (instruction-operand instruction)
     (assemble-operand instruction (assemble-expression operand-expression)))
  (instruction-optimize-addrmode instruction)
  instruction)

(fn assemble-label (x)
  (add-label .x *pc*)
  x)

(fn assemble-assignment (x)
  (add-label .x. (assemble-expression ..x.))
  x)

(fn assemble-identifier (x)
  (| (get-label .x :required? (not (first-pass?)))
     0))

(fn assemble-toplevel-expression (x)
  (!? (assemble-expression x :ensure? t :not-zero? t)
      (? (cons? !)
         (? (| (number? !.)
               (character? !.))
            !
            (assemble-parsed-expressions !))
         !)))

(fn update-pc (x pc)
  (+ pc
     (?
       (not x)                  0
       (number? x)              1
       (character? x)           1
       (string? x)              (length x)
       (instruction? x)         (instruction-size x)
       (& (cons? x)
          (| (number? x.)
             (character? x.)))  (length x)
       0)))

(fn catch-end (x)
  (& (cons? x)
     (eq x. 'directive)
     (eq .x. 'end)
     (assemble-end nil)))

(fn assemble (x)
  (? *disabled?*
     (catch-end x)
     (aprog1 (?
               (not x)           x
               (string? x)       x
               (number? x)       x
               (character? x)    x
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

(fn assemble-parsed-expressions (x)
  (@ [with-temporary *assembler-current-line* _
       (| *assign-blocks-to-segments?*
          (let-when b *sourceblock-stack*.
            (enqueue (sourceblock-exprs b) _)))
       (. _.
          (? (cons? ._)
             (@ #'assemble ._)
             (assemble ._)))]
     x))

(fn assemble-pass (x &key unassigned-segment-blocks segments description)
  (format t "Pass ~A~A…~%" *pass* description)
  (= *label-changed?* nil
     *unassigned-segment-blocks* unassigned-segment-blocks
     *segments* segments
     *pc*       0
     *acycles*  0)
  (rewind-labels)
  (aprog1 (assemble-parsed-expressions x)
    (++! *pass*)))

(fn assemble-multiple-passes (x &key (unassigned-segment-blocks nil)
                                     (segments (make-queue))
                                     (description ""))
  (clear-labels)
  (= *pass* 0)
  (prog1 (while (| *label-changed?*
                   (< *pass* 4))
                (assemble-pass x :unassigned-segment-blocks unassigned-segment-blocks
                                 :segments segments
                                 :description description)
           (assemble-pass x :unassigned-segment-blocks unassigned-segment-blocks
                            :segments segments
                            :description description))
    (!? *sourceblock-stack*
        (assembler-error "Block(s) with no END: ~A" !))))
