; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun assemble-expression (x &key (ensure? nil) (not-zero? nil))
  (| (& (number? x) x)
     (case x.
       'expression  (unless (& (not ensure?)
                               (first-pass?))
                      (eval (macroexpand (labels-to-exprs .x))))
       'identifier  (get-label .x :required? (not (first-pass?))))
     (unless not-zero?
       0)))

(def-instruction assemble-operand (instruction x)
  (when (character? x)
    (= operand (char-code x)))
  (when (eq 'branch addrmode)
    (= x (- x *pc* 1))
    (& (< 2 *pass*)
       (| (< x -128)
          (> x 127))
       (assembler-error "Branch is out of range (~A bytes)." x)))
  (& (eq x 'jmp)
     (<= -128 (- x *pc* 1) 127)
     (assembler-hint "JMP is short enough for a branch."))
  (= (instruction-operand instruction) x))

(defun assemble-instruction (instruction)
  (= (instruction-address instruction) *pc*)
  (= (instruction-operand instruction)
     (assemble-expression (instruction-operand-expression instruction)))
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
       (number? x)       1
       (string? x)       (length x)
       (instruction? x)  (instruction-size x)
       0)))

(defun assemble (x)
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
    (= *pc* (update-pc ! *pc*))))

(defun assemble-parsed-expressions (x)
  (@ [with-temporary *assembler-current-line* _
       (| *assign-blocks-to-segments?*
          (let-when b *sourceblock-stack*.
            (enqueue (sourceblock-exprs b) _)))
       (. _.
          (? (cons? ._)
             (@ [? *disabled?*
                   (? (& (cons? _)
                         (eq _. 'directive)
                         (eq ._. 'end))
                      (assemble-end nil))
                   (assemble _)]
                ._)
             (assemble ._)))]
     x))

(defun assemble-pass (x &key unassigned-segment-blocks segments)
  (format t "Pass ~A…~%" *pass*)
  (= *label-changed?* nil
     *unassigned-segment-blocks* unassigned-segment-blocks
     *segments* segments
     *pc*       0
     *acycles*  0)
  (rewind-labels)
  (aprog1 (assemble-parsed-expressions x)
    (++! *pass*)))

(defun assemble-multiple-passes (x &key (unassigned-segment-blocks nil)
                                        (segments (make-queue)))
  (clear-labels)
  (= *pass* 0)
  (prog1 (while (| *label-changed?*
                   (< *pass* 4))
                (assemble-pass x :unassigned-segment-blocks unassigned-segment-blocks
                                 :segments segments)
           (assemble-pass x :unassigned-segment-blocks unassigned-segment-blocks
                            :segments segments))
    (!? *sourceblock-stack*
        (assembler-error "Block(s) with no END: ~A" !))))

(defun assemble-parsed-files (x)
  (= x (assemble-multiple-passes x))
  (unless *unassigned-segment-blocks*
    (return x))
  (format t "Assembling again to assign BLOCKs to SEGMENTS…~%")
  (sort-unassigned-segment-blocks)
  (with-temporary *assign-blocks-to-segments?* t
    (assemble-multiple-passes x :unassigned-segment-blocks *unassigned-segment-blocks*
                                :segments *segments*)))

(defun assemble-files (out-name &rest in-names)
  (with-temporary *unassigned-segment-blocks* nil
    (let dump-name (+ out-name ".lst")
      (format t "Assembling to '~A'. Dump file is '~A'…~%" out-name dump-name)
      (print (assemble-parsed-files (parse-files in-names))))
    (check-on-unassigned-blocks)
    (rewind-labels))
  nil)
