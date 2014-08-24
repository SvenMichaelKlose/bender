;;;;; bender – Copyright (c) 2014 Sven Michael Klose <pixel@copei.de>

(defvar *pc* nil)
(defvar *pass* nil)

(defun assemble-mnemonic-addrmode (mnemonic addrmode)
  (opcode-instruction (generate-opcode mnemonic addrmode)))

(defun assemble-expression (x)
  (case x. :test #'eq
    'number      .x
    'identifier  (get-label .x)
    'expression  (? (zero? *pass*)
                    0
                    (eval (macroexpand (labels-to-exprs .x))))))

(defun assemble-operand (out inst operand)
  (when (eq 'branch (instruction-addrmode inst))
    (= operand (- operand *pc* 1)))
  (dotimes (i (instruction-operand-size inst))
    (princ (code-char (mod operand 256)) out)
    (++! *pc*)
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
    (princ (code-char (instruction-opcode inst)) out)
    (++! *pc*)
    (assemble-operand out inst operand)))

(defun assemble-assignment (x)
  (add-label .x. (assemble-expression ..x.)))

(defun assemble-string (out x)
  (princ x out)
  (+! *pc* (length x)))

(defun assemble-number (out x)
  (princ (code-char x) out)
  (++! *pc*))

(defun assemble-identifier (out x)
  (princ (code-char (get-label x)) out)
  (++! *pc*))

(defun assemble-toplevel-expression (out x)
  (princ (code-char (assemble-expression x)) out)
  (++! *pc*))

(defun assemble-fill (x)
  (when (< 1 *pass*)
    (adotimes ((assemble-expression ..x.))
      (princ (code-char 0) out)
      (++! *pc*))))

(defun assemble-directive (x)
  (case .x. :test #'eq
    'org          (= *pc* (assemble-expression ..x.))
    'open_scope   (open-scope)
    'close_scope  (close-scope)
    'fill         (assemble-fill x)
    (error "Unsupported directive ~A." x)))

(defun assemble-list (out x)
  (adolist x
    (format t "Assembling ~A.~%" !)
    (case !. :test #'eq
      'label       (add-label .! *pc*)
      'mnemonic    (apply #'assemble-instruction
                          out (list .!. ..!. (assemble-expression ...!.)))
      'assignment  (assemble-assignment !)
      'directive   (assemble-directive !)
      'string      (assemble-string out .!)
      'number      (assemble-number out .!)
      'expression  (assemble-toplevel-expression out !)
      'identifier  (assemble-identifier out .!)
      (error "Unexpected parsed line ~A." !))))

(defun assemble-pass (out x)
  (= *pc* 0)
  (format t "Pass ~A...~%" (++ *pass*))
  (assemble-list out x))

(defun assemble-files (out-name &rest in-names)
  (let i (apply #'+ (filter [(format t "Parsing '~A'.~%" _)
                             (with-input-file i _
                               (parse-stream i))]
                            in-names))
    (format t "Parsed input:~%")
    (print i)
    (clear-labels)
    (= *pass* 0)
    (with-output-file o "/dev/null"
      (assemble-pass o i))
    (= *scopes* (reverse *scopes*))
    (= *label-changed?* t)
    (alet *pc*
      (while *label-changed?*
             nil
        (++! *pass*)
        (= *scope* *scopes*)
        (= *label-changed?* nil)
        (with-output-file o out-name
          (assemble-pass o i))))))
