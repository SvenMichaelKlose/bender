; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defvar *assembler-current-line* nil)
(defvar *assembler-output-stream* nil)
(defvar *assembler-output-instructions* nil)
(defvar *assembler-dump-stream* nil)
(defvar *pc* nil)
(defvar *pass* nil)
(defvar *disabled?* nil)
(defvar *data?* nil)
(defvar *cycles* nil)
(defvar *acycles* 0)

(defstruct sourceblock
  name
  returner
  (exprs (make-queue))
  pc-start
  pc-end)

(def-sourceblock sourceblock-size (sourceblock)
  (- pc-end pc-start))

(defvar *sourceblock-stack* nil)

(defstruct segment
  size
  (may-be-shorter? nil)
  (sourceblocks nil))

(defvar *segments* nil)
(defvar *unassigned-segment-blocks* nil)
(defvar *assign-blocks-to-segments?* nil)

(defun assembler-error (x &rest fmt)
  (alet *assembler-current-line*.
    (error (+ (when (cadr !)
                (format nil "~LError while assembling '~A', line ~A:~%~A"
                            (cadr !) (cddr !) !.))
              "~A")
           (apply #'format nil x fmt))))

(defun assembler-hint (x &rest fmt)
  (when (< 2 *pass*)
    (alet *assembler-current-line*.
      (when (cadr !)
        (format t (+ (format nil "~LHint for '~A', line ~A:~%~A"
                                 (cadr !) (cddr !) !.)
                     "~A~%")
                  (apply #'format nil x fmt))))))

(defun print-dump-header (o)
  (format o ";~%")
  (format o "; Pass ~A~%" *pass*)
  (format o ";~%")
  (format o "; Adress | Cycles | Accumulated cycles | Bytes | Source~%"))

(defun first-pass? ()
  (< *pass* 1))

(defun assemble-mnemonic-addrmode (mnemonic addrmode)
  (opcode-instruction (generate-opcode mnemonic addrmode)))

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

(defun assemble-byte (x)
  (++! *pc*)
  (unless *data?*
    x))

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

(defun assemble-instruction (mnemonic addrmode operand)
  (aprog1 (assemble-mnemonic-addrmode mnemonic addrmode)
    (= (instruction-address !) *pc*)
    (= (instruction-operand !) operand)
    (instruction-optimize-addrmode !)
    (= *cycles* (instruction-cycles !))))

(defun assemble-assignment (x)
  (add-label .x. (assemble-expression ..x.)))

(defun assemble-string (x)
  (string-list x))

(defun assemble-identifier (x)
  (| (get-label x :required? (not (first-pass?)))
     0))

(defun assemble-toplevel-expression (x)
  (awhen (assemble-expression x :ensure? t :not-zero? t)
    (?
      (cons? !)    (? (number? !.)
                      !
                      (assemble-parsed-expressions !))
      (string? !)  (assemble-string !)
      !)))

(defun assemble-fill (x)
  (when (< 1 *pass*)
    (maptimes [identity 0] (assemble-expression ..x.))))

(defun make-returner ()
  (with (disabled?  *disabled?*
         data?      *data?*)
    [= *disabled?* disabled?
       *data?*     data?]))

(defun push-sourceblock (name)
  (push (make-sourceblock :name name
                          :returner (make-returner))
        *sourceblock-stack*))

(defun assemble-if (x)
  (| ..x
     (assembler-error "IF expects a Lisp expression."))
  (push-sourceblock 'if)
  (= *disabled?* (not (assemble-expression ..x. :ensure? t :not-zero? t)))
  nil)

(defun assemble-data (x)
  (push-sourceblock 'data)
  (= *data?* t)
  nil)

(defun make-block-returner ()
  (alet (make-returner)
    [(| *assign-blocks-to-segments?*
        (push _ *unassigned-segment-blocks*))
     (funcall ! _)]))

(defun assemble-block (x)
  (push (make-sourceblock :name 'block
                          :returner (make-block-returner)
                          :pc-start *pc*)
        *sourceblock-stack*)
  (= *disabled?* *assign-blocks-to-segments?*)
  nil)

(defun fill-up-remaining-segment (bytes-left)
  (unless (zero? bytes-left)
    (format t "Filling up remaining segment space with ~A zeroes.~%" bytes-left)
    (maptimes [identity 0] bytes-left)))

(defun assign-segment-block (bytes-left b may-be-shorter?)
  (= *unassigned-segment-blocks* (remove b *unassigned-segment-blocks* :test #'eq))
  (format t "Assigned block of size ~A.~%" (sourceblock-size b))
  (+ (assemble-parsed-expressions (butlast (queue-list (sourceblock-exprs b))))
     (try-to-assign-segment-block (- bytes-left (sourceblock-size b)) may-be-shorter?)))

(defun try-to-assign-segment-block (bytes-left may-be-shorter?)
  (!? (& (< 9 bytes-left)
         (find-if [<= (sourceblock-size _) bytes-left] *unassigned-segment-blocks*))
      (assign-segment-block bytes-left ! may-be-shorter?)
      (? may-be-shorter?
         (format t "Trimmed segment by ~A bytes.~%" bytes-left)
         (fill-up-remaining-segment bytes-left))))

(defun fill-segment (size may-be-shorter?)
  (& (zero? size)
     (assembler-error "SEGMENT size must be larger than 0."))
  (format t "Filling up segment of size ~a…~%" size)
  (try-to-assign-segment-block size may-be-shorter?))

(defun segment (&key size (may-be-shorter? nil))
  (| (number? size)
     (assembler-error "SEGMENT expects a size."))
  (? *assign-blocks-to-segments?*
     (fill-segment *assembler-output-stream* size may-be-shorter?)
     (enqueue *segments* (make-segment :size size
                                       :may-be-shorter? may-be-shorter?)))
  nil)

(defun assemble-end (x)
  ; TODO a proper parser would do wonders here.
  (| *sourceblock-stack*
     (assembler-error "Unexpected END. No block open."))
  (& ...x
     (assembler-error "END doesn't expect more than one optional argument. (IF, DATA or BLOCK.)"))
  (| (in? (cdr ..x.) nil 'if 'data 'block)
     (assembler-error "END expects IF, DATA or BLOCK as an argument."))
  (with (b        (pop *sourceblock-stack*)
         name     (cdr ..x.)
         expected (sourceblock-name b))
    (when name
      (| (eq name expected)
         (assembler-error "Unexpected END of ~A. Expected ~A."
                          name expected)))
    (= (sourceblock-pc-end b) *pc*)
    (funcall (sourceblock-returner b) b))
  nil)

(defun assemble-directive (x)
  (case .x.
    'org      (= *pc* (assemble-expression ..x.))
    'fill     (assemble-fill x)
    'if       (assemble-if x)
    'data     (assemble-data x)
    'block    (assemble-block x)
    'end      (assemble-end x)
    (assembler-error "Unsupported directive ~A." x)))

(defun assemble (x)
  (?
    (string? x)  (assemble-string x)
    (number? x)  x
    (case x.
      'label        x ;(add-label .x *pc*)
      'instruction  (funcall #'assemble-instruction .x. ..x. (assemble-expression ...x.))
      'assignment   x ;(assemble-assignment x)
      'directive    x ;(assemble-directive x)
      'identifier   x ;(assemble-identifier .x)
      'expression   x ;(assemble-toplevel-expression x)
      (assembler-error "Unexpected parsed expression ~A." x))))

(defun assemble-dump-line (pc bytes)
  (let o *assembler-dump-stream*
    (fresh-line o)
    (print-hexword pc o)
    (princ ":" o)
    (format o " ~A ~A :" (| *cycles* " ") *acycles*)
    (& *cycles* (+! *acycles* *cycles*))
    (adolist ((string-list bytes))
      (princ " " o)
      (print-hexbyte (char-code !) o))
    (while (< (stream-location-column (stream-output-location o)) 26)
           nil
       (princ " " o))
    (princ *assembler-current-line*.. o)))

(defun assemble-parsed-expressions (x)
  (mapcan [with-temporary *assembler-current-line* _
            (| *assign-blocks-to-segments?*
               (let-when b (car *sourceblock-stack*)
                 (enqueue (sourceblock-exprs b) _)))
            (@ [? *disabled?*
                  (? (& (cons? _)
                        (eq _. 'directive)
                        (eq ._. 'end))
                     (assemble-end nil))
                  (assemble _)]
               ._)]
          x))

(defun assemble-pass (x)
  (= *pc* 0)
  (= *acycles* 0)
  (rewind-labels)
  (assemble-parsed-expressions x))

(defun assemble-pass-to-file (out-name dump-name i)
  (with-output-file out out-name
    (with-output-file dump dump-name
      (with-temporaries (*assembler-output-instructions* (make-queue)
                         *assembler-dump-stream* dump)
        (print-dump-header dump)
        (print (assemble-pass i))))))

(defun sort-unassigned-segment-blocks ()
  (= *unassigned-segment-blocks* (sort *unassigned-segment-blocks*
                                       :test #'((a b)
                                                 (>= (sourceblock-size a)
                                                     (sourceblock-size b))))))

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
    (assemble-pass-to-file out-name dump-name x)
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

(defun check-on-unassigned-blocks ()
  (awhen *unassigned-segment-blocks*
    (assembler-error "~A BLOCKs couldn't get assigned to SEGMENTs (~A bytes). Remaining blocks: ~A"
                     (length *unassigned-segment-blocks*)
                     (apply #'+ (@ #'sourceblock-size *unassigned-segment-blocks*))
                     *unassigned-segment-blocks*)))

(defun assemble-files (out-name &rest in-names)
  (with-temporary *unassigned-segment-blocks* nil
    (let dump-name (+ out-name ".lst")
      (format t "Assembling to '~A'. Dump file is '~A'…~%"
              out-name dump-name)
      (assemble-parsed-files out-name dump-name (parse-files in-names)))
    (check-on-unassigned-blocks)
    (rewind-labels))
  nil)
