; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defvar *pc* nil)
(defvar *pass* nil)

(defun first-pass? ()
  (zero? *pass*))

(defun assemble-mnemonic-addrmode (mnemonic addrmode)
  (opcode-instruction (generate-opcode mnemonic addrmode)))

(defun assemble-expression (x)
  (| (& (number? x) x)
     (case x.
       'expression  (unless (first-pass?)
                      (eval (macroexpand (labels-to-exprs .x))))
       'identifier  (get-label .x :required? (not (first-pass?))))
     0))

(defun dump-byte (x)
  (print-hexbyte x)
  (princ " ")
  x)

(defun assemble-byte (x out)
  (princ (code-char (dump-byte x)) out)
  (++! *pc*))

(defun assemble-operand (out inst operand)
  (when (eq 'branch (instruction-addrmode inst))
    (= operand (- operand *pc* 1)))
  (dotimes (i (instruction-operand-size inst))
    (assemble-byte (mod operand 256) out)
    (= operand (>> operand 8))))

(defun check-branch-range (inst operand)
  (& (< 1 *pass*)
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
    (assemble-byte (instruction-opcode inst) out)
    (assemble-operand out inst operand)))

(defun assemble-assignment (x)
  (add-label .x. (assemble-expression ..x.)))

(defun assemble-string (out x)
  (adolist ((string-list x))
    (assemble-byte (char-code !) out)))

(defun assemble-number (out x)
  (assemble-byte x out))

(defun assemble-identifier (out x)
  (assemble-byte (| (get-label x :required? (not (first-pass?)))
                       0)
                 out))

(defun assemble-toplevel-expression (out x)
  (assemble-byte (assemble-expression x) out))

(defun assemble-fill (out x)
  (when (< 1 *pass*)
    (adotimes ((assemble-expression ..x.))
      (assemble-byte 0 out))))

(defun assemble-directive (out x)
  (case .x.
    'org   (= *pc* (assemble-expression ..x.))
    'fill  (assemble-fill out x)
    (error "Unsupported directive ~A." x)))

(defun assemble-list (out x)
  (adolist x
    (format t "~LAssembling: ~A~%" !)
    (print-hexword *pc*)
    (princ ": ")
    (?
      (string? !)  (assemble-string out !)
      (number? !)  (assemble-number out !)
      (case !.
        'label        (add-label .! *pc*)
        'instruction  (funcall #'assemble-instruction
                               out .!. ..!. (assemble-expression ...!.))
        'assignment   (assemble-assignment !)
        'directive    (assemble-directive out !)
        'identifier   (assemble-identifier out .!)
        'expression   (assemble-toplevel-expression out !)
        (error "Unexpected parser expression ~A." !)))))

(defun assemble-pass (out x)
  (= *pc* 0)
  (format t "Pass ~A...~%" (++ *pass*))
  (rewind-labels)
  (assemble-list out x)
  (fresh-line))

(defun assemble-pass-to-file (name i)
  (with-output-file o name
    (assemble-pass o i)))

(defun assemble-files (out-name &rest in-names)
  (let i (apply #'+ (filter [(format t "Parsing '~A'.~%" _)
                             (with-input-file i _
                               (parse-stream i))]
                            in-names))
    (clear-labels)
    (= *pass* 0)
    (alet *pc*
      (while (| *label-changed?*
                (< *pass* 2))
             nil
        (= *label-changed?* nil)
        (assemble-pass-to-file out-name i)
        (++! *pass*)))))
