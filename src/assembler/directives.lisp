; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defvar *directives* nil)

(defun directive? (x)
  (assoc x *directives* :test #'eq))

(defmacro define-directive (name arg &body body)
  (print-definition `(define-directive ,name))
  (& (directive? name)
     (error "Directive ~A has been defined already." name))
  `(acons! ',name #'((,arg) ,@body) *directives*))

(defun assemble-directive (x)
  (!? (directive? .x.)
      (funcall .! x)
      (assembler-error "Unsupported directive ~A." x)))

(define-directive org x
  (= *pc* (assemble-expression ..x.))
  nil)

(define-directive fill x
  (when (< 1 *pass*)
    (alet (assemble-expression ..x.)
      (& (< ! 0)
         (assembler-error "Cannot fill minus ~A bytes." (abs !)))
      (maptimes [identity 0] !))))

(define-directive if x
  (| ..x
     (assembler-error "IF expects a Lisp expression."))
  (push-sourceblock 'if)
  (= *disabled?* (not (assemble-expression ..x. :ensure? t :not-zero? t)))
  nil)

(define-directive data x
  (push-sourceblock 'data)
  (= *data?* t)
  nil)

(defun assemble-end (x)
  ; TODO a proper parser would do wonders here.
  (| *sourceblock-stack*
     (assembler-error "Unexpected END. No block open."))
  (& ...x
     (assembler-error "END doesn't expect more than one optional argument. (IF, DATA or BLOCK.)"))
  (| (in? (cdr ..x.) nil 'if 'data 'block)
     (assembler-error "END expects IF, DATA or BLOCK or no argument."))
  (with (b         (pop *sourceblock-stack*)
         name      (cdr ..x.)
         expected  (sourceblock-name b))
    (!? name
        (| (eq ! expected)
           (assembler-error "Unexpected END of ~A. Expected ~A." ! expected)))
    (= (sourceblock-pc-end b) *pc*)
    (funcall (sourceblock-returner b) b))
  nil)

(define-directive end x
  (assemble-end x))
