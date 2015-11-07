; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defvar *current-line* nil)
(defvar *pc* nil)
(defvar *pass* nil)
(defvar *disabled?* nil)
(defvar *data?* nil)
(defvar *block-stack* nil)
(defvar *cycles* nil)
(defvar *acycles* 0)

(defun first-pass? ()
  (< *pass* 1))

(defun assemble-mnemonic-addrmode (mnemonic addrmode)
  (opcode-instruction (generate-opcode mnemonic addrmode)))

(defmacro asm (&rest x)
  (| (every #'string? x)
     (error "ASM expects one or more string."))
  `'(,@(mapcan [parse-string (format nil "~A~%" _)] x)))

(defun assemble-expression (x &key (ensure? nil))
  (| (& (number? x) x)
     (case x.
       'expression  (unless (& (not ensure?)
                               (first-pass?))
                      (eval (macroexpand (labels-to-exprs .x))))
       'identifier  (get-label .x :required? (not (first-pass?))))
     0))

(defun assemble-byte (out x)
  (| *data?*
     (princ (code-char x) out))
  (++! *pc*))

(defun assemble-operand (out inst operand)
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
          (error "Branch out of range (~A)." !))))))

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

(defun assemble-if (out x)
  (push *disabled?* *block-stack*)
  (push *data?* *block-stack*)
  (= *disabled?* (zero? (assemble-expression ..x. :ensure? t))))

(defun assemble-data (out x)
  (push *disabled?* *block-stack*)
  (push *data?* *block-stack*)
  (= *data?* t))

(defun assemble-end ()
  (| *block-stack*
     (error "Unexpected directive 'end'."))
  (= *data?* (pop *block-stack*))
  (= *disabled?* (pop *block-stack*)))

(defun assemble-directive (out x)
  (case .x.
    'org   (= *pc* (assemble-expression ..x.))
    'fill  (assemble-fill out x)
    'if    (assemble-if out x)
    'data  (assemble-data out x)
    'end   (assemble-end)
    (error "Unsupported directive ~A." x)))

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
      (error "Unexpected parser expression ~A." x))))

(defun assemble-and-dump (out x line)
  (= *cycles* nil)
  (with-string-stream o
    (let pc *pc*
      (assemble o x)
      (let bytes (get-stream-string o)
        (when bytes
          (fresh-line)
          (print-hexword pc)
          (princ ":")
          (format t " ~A ~A :" *acycles* (| *cycles* " "))
          (& *cycles* (+! *acycles* *cycles*))
          (adolist ((string-list bytes))
            (princ " ")
            (print-hexbyte (char-code !))))
        (while (< (stream-location-column (stream-output-location *standard-output*)) 26)
               nil
          (princ " "))
        (princ line)
        (princ bytes out)))))

(defun assemble-parsed-expressions (out x)
  (adolist x
    (let line !.
      (adolist (.!)
          (? *disabled?*
             (? (equal ! '(directive end))
                (assemble-end))
             (assemble-and-dump out ! line))))))

(defun assemble-pass (out x)
  (= *pc* 0)
  (= *acycles* 0)
  (rewind-labels)
  (assemble-parsed-expressions out x))

(defun assemble-pass-to-file (name i)
  (with-output-file out name
    (assemble-pass out i)))

(defun assemble-files (out-name &rest in-names)
  (format t "Assembling to '~A'…~%" out-name)
  (let parsed (parse-files in-names)
    (clear-labels)
    (= *pass* 0)
    (alet *pc*
      (while (| *label-changed?*
                (< *pass* 3))
             nil
        (= *label-changed?* nil)
        (assemble-pass-to-file out-name parsed)
        (++! *pass*))))
  (rewind-labels)
  nil)
