; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defvar *assembler-current-line* nil)
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

(defvar *unassigned-segment-blocks* nil)
(defvar *fill-up-segments?* nil)

(defun assembler-error (x &rest fmt)
  (alet *assembler-current-line*.
    (error "~LError while assembling '~A', line ~A:~%~A~A"
           (cadr !) (cddr !)
           !.
           (apply #'format nil x fmt))))

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

(defun assemble-byte (out x)
  (| *data?*
     (write-byte x out))
  (++! *pc*))

(defun assemble-operand (out inst operand)
  (when (character? operand)
    (= operand (char-code operand)))
  (when (eq 'branch (instruction-addrmode inst))
    (= operand (- operand *pc* 1)))
  (dotimes (i (instruction-operand-size inst))
    (assemble-byte out (mod operand 256))
    (= operand (>> operand 8))))

(defun check-branch-range (inst operand)
  (& (< 3 *pass*)
     (eq 'branch (instruction-addrmode inst))
     (alet (- operand *pc*)
       (& (| (< ! -128)
             (< 255 !))
          (progn
            (print *pc*)
            (print operand)
          (assembler-error "Branch out of range (~A)." !))))))

(defun assemble-instruction (out mnemonic addrmode operand)
  (let inst (assemble-mnemonic-addrmode mnemonic addrmode)
    (= (instruction-operand inst) operand)
    (check-branch-range inst operand)
    (instruction-optimize-addrmode inst)
    (assemble-byte out (instruction-opcode inst))
    (assemble-operand out inst operand)
    (= *cycles* (instruction-cycles inst))))

(defun assemble-assignment (x)
  (add-label .x. (assemble-expression ..x.)))

(defun assemble-string (out x)
  (adolist ((string-list x))
    (assemble-byte out (char-code !))))

(defun assemble-number (out x)
  (assemble-byte out x))

(defun assemble-identifier (out x)
  (assemble-byte out (| (get-label x :required? (not (first-pass?)))
                        0)))

(defun assemble-toplevel-expression (out x)
  (alet (assemble-expression x)
    (?
      (cons? !)    (? (number? !.)
                      (adolist !
                        (assemble-byte out !))
                      (assemble-parsed-expressions out !))
      (string? !)  (assemble-string out !)
      (assemble-byte out !))))

(defun assemble-fill (out x)
  (when (< 1 *pass*)
    (adotimes ((assemble-expression ..x.))
      (assemble-byte out 0))))

(defun make-returner ()
  (with (disabled?  *disabled?*
         data?      *data?*)
    [= *disabled?* disabled?
       *data?*     data?]))

(defun push-sourceblock (name)
  (push (make-sourceblock :name name
                          :returner (make-returner))
        *sourceblock-stack*))

(defun assemble-if (out x)
  (push-sourceblock 'if)
  (= *disabled?* (not (assemble-expression ..x. :ensure? t :not-zero? t))))

(defun assemble-data (out x)
  (push-sourceblock 'data)
  (= *data?* t))

(defun make-block-returner ()
  (alet (make-returner)
    [(push _ *unassigned-segment-blocks*)
     (funcall ! _)]))

(defun assemble-block (out x)
  (push (make-sourceblock :name 'block
                          :returner (make-block-returner)
                          :pc-start *pc*)
        *sourceblock-stack*))

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
    (funcall (sourceblock-returner b) b)))

(defun assemble-directive (out x)
  (case .x.
    'org    (= *pc* (assemble-expression ..x.))
    'fill   (assemble-fill out x)
    'if     (assemble-if out x)
    'data   (assemble-data out x)
    'block  (assemble-block out x)
    'end    (assemble-end x)
    (assembler-error "Unsupported directive ~A." x)))

(defun assemble (out x)
  (?
    (string? x)  (assemble-string out x)
    (number? x)  (assemble-number out x)
    (case x.
      'label        (add-label .x *pc*)
      'instruction  (funcall #'assemble-instruction
                             out .x. ..x. (assemble-expression ...x.))
      'assignment   (assemble-assignment x)
      'directive    (assemble-directive out x)
      'identifier   (assemble-identifier out .x)
      'expression   (assemble-toplevel-expression out x)
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

(defun assemble-and-dump (out x)
  (= *cycles* nil)
  (with-string-stream o
    (let pc *pc*
      (assemble o x)
      (let bytes (get-stream-string o)
        (assemble-dump-line pc bytes)
        (unless (zero? (length bytes))
          (princ bytes out))))))

(defun assemble-parsed-expressions (out x)
  (adolist x
    (with-temporary *assembler-current-line* !
      (adolist (.!)
        (let-when b (car *sourceblock-stack*)
          (enqueue (sourceblock-exprs b) !))
        (? *disabled?*
           (? (equal ! '(directive end)) ; TODO end with block type
              (assemble-end nil))
           (assemble-and-dump out !))))))

(defun assemble-pass (out x)
  (= *pc* 0)
  (= *acycles* 0)
  (rewind-labels)
  (assemble-parsed-expressions out x))

(defun print-dump-header (o)
  (format o ";~%")
  (format o "; Pass ~A~%" *pass*)
  (format o ";~%")
  (format o "; Adress | Cycles | Accumulated cycles | Bytes | Source~%"))

(defun assemble-pass-to-file (out-name dump-name i)
  (with-output-file out out-name
    (with-output-file dump dump-name
      (with-temporary *assembler-dump-stream* dump
        (print-dump-header dump)
        (assemble-pass out i)))))

(defun assemble-parsed-files (out-name dump-name x)
  (clear-labels)
  (= *pass* 0)
  (while (| *label-changed?*
            (< *pass* 3))
         nil
    (format t "Pass ~A…~%" *pass*)
    (= *label-changed?* nil)
    (assemble-pass-to-file out-name dump-name x)
    (++! *pass*)))

(defun assemble-files (out-name &rest in-names)
  (with-temporary *unassigned-segment-blocks* nil
    (let dump-name (+ out-name ".lst")
      (format t "Assembling to '~A'. Dump file is '~A'…~%"
              out-name dump-name)
      (assemble-parsed-files out-name dump-name (parse-files in-names)))
    (rewind-labels))
  nil)
