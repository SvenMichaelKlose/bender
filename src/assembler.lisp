; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defvar *current-line* nil)
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

(defun assemble-byte (out x)
  (princ (code-char x) out)
  (++! *pc*))

(defun assemble-operand (out inst operand)
  (when (eq 'branch (instruction-addrmode inst))
    (= operand (- operand *pc* 1)))
  (dotimes (i (instruction-operand-size inst))
    (assemble-byte out (mod operand 256))
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
    (assemble-byte out (instruction-opcode inst))
    (assemble-operand out inst operand)))

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
    (? (cons? !)
       (assemble-list out !)
       (assemble-byte out !))))

(defun assemble-fill (out x)
  (when (< 1 *pass*)
    (adotimes ((assemble-expression ..x.))
      (assemble-byte out 0))))

(defun assemble-directive (out x)
  (case .x.
    'org   (= *pc* (assemble-expression ..x.))
    'fill  (assemble-fill out x)
    (error "Unsupported directive ~A." x)))

(defun assemble-list (out x)
  (adolist x
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
  (rewind-labels)
  (assemble-list out x)
  (fresh-line))

(defun assemble-pass-to-file (name i)
  (with-output-file o name
    (assemble-pass o i)))

(defun assemble-files (out-name &rest in-names)
  (let parsed (parse-files in-names)
    (clear-labels)
    (= *pass* 0)
    (alet *pc*
      (while (| *label-changed?*
                (< *pass* 2))
             nil
        (= *label-changed?* nil)
        (assemble-pass-to-file out-name (apply #'+ (cdrlist parsed)))
        (++! *pass*)))))
