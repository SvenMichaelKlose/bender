; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defvar *parser-stream* nil)

(defun parser-error (x &rest fmt)
  (alet (stream-input-location *parser-stream*)
    (error "~LError while parsing '~A', line ~A: ~A"
           (stream-location-id !)
           (stream-location-line !)
           (apply #'format nil x fmt))))

(def-head-predicate identifier)
(def-head-predicate colon)
(def-head-predicate hash)
(def-head-predicate comma)
(def-head-predicate bracket-open)
(def-head-predicate bracket-close)
(def-head-predicate assignment)
(def-head-predicate expression)
(def-head-predicate label)

(defun parse-assignment (x)
  (| (assignment? .x.)
     (parser-error "Assignment '=' expected instead of ~A." .x))
  `((assignment ,(cdr x.) ,..x.)))

(defun parse-identifier (x)
  (? (assignment? .x.)
     (parse-assignment x)
     (. x. (parse-0 .x))))

(defun parse-directive (x)
  `((,(car x.) ,(cdr x.) ,@.x)))

(defun parse-labels (x)
  (& x
     (? (& (identifier? x.)
           (colon? .x.))
        (. (. 'label (cdr x.))
           (parse-labels ..x))
        (. x. (parse-labels .x)))))

(defun operand-expression? (x)
  (| (number? x)
     (identifier? x)
     (expression? x)))

(defun parse-operand-immediate (x)
  (| (operand-expression? .x.)
     (parser-error "Expression expected instead of ~A."
                   (car .x.)))
  (values 'imm .x.))

(defun parse-operand-absolute (x)
  (? (not .x)
     (values 'abs x.)
     (? (comma? .x.)
        (? (identifier? ..x.)
           (values (case (cdr ..x.)
                     'x  'absx
                     'y  'absy
                     (parser-error "Index register expected instead of '~A'."
                                   (cdr ..x.)))
                   x.)
           (parser-error "Index register expected."))
        (parser-error "Comma expected instead of ~A."
                      (car .x.)))))

(defun parse-operand-indexed-indirect (x)
  (?
    (comma? ...x.)  (? (eq 'y (cdr ....x.))
                       (values 'izpy .x.)
                       (parser-error "Index register Y expected."))
    (parser-error "Comma or end of line expected instead of ~A."
                  (car ...x.))))

(defun parse-operand-indirect-indexed (x)
  (? (identifier? ...x.)
     (? (eq 'x (cdr ...x.))
        (? (bracket-close? ....x.)
           (values 'izpx .x.)
           (parser-error "Closing bracket expected instead of ~A."
                         ....x.))
        (parser-error "Index register X expected."))
     (parser-error "Index register X expected.")))

(defun parse-operand-indirect (x)
  (| (operand-expression? .x.)
     (parser-error "Expression expected instead of ~A."
                   (car .x.)))
  (?
    (bracket-close? ..x.)
      (? (not ...x)
         (values 'indi .x.)
         (parse-operand-indexed-indirect x))

    (comma? ..x.)
      (parse-operand-indirect-indexed x)
   (parser-error "~A closing bracket."
                 (? (member-if #'bracket-close? ...x)
                    "Misplaced"
                    "Missing"))))

(defun parse-operand (x)
  (?
    (not x)             (values 'accu nil)
    (& (cons? x.)
       (eq 'hash x..))        (parse-operand-immediate x)
    (operand-expression? x.)  (parse-operand-absolute x)
    (bracket-open? x.)        (parse-operand-indirect x)
    (parser-error "Syntax error at ~A." x.)))

(defun parse-instruction (x)
  (with ((addrmode operand-expression) (parse-operand .x))
    (list (make-instruction :mnemonic (cdr x.)
                            :addrmode addrmode
                            :operand-expression operand-expression))))

(defun parse-0 (x)
  (when x
    (?
      (number? x.)     x
      (character? x.)  x
      (string? x.)     x
      (case x.. :test #'eq
        'expression  (. x. (parse-0 .x))
        'label       (. x. (parse-0 .x))
        'directive   (parse-directive x)
        'mnemonic    (parse-instruction x)
        'identifier  (parse-identifier x)
        (parser-error "Unexpected token ~A." x.)))))

(defun parse (x)
  (parse-0 (parse-labels x)))

(defun parse-stream (i)
  (with-temporary *parser-stream* i
    (with-queue q
      (while (peek-char i)
             (queue-list q)
        (with (ci       (make-copying-stream :in i)
               file-id  (stream-location-id (stream-input-location i))
               line-nr  (stream-location-line (stream-input-location i))
               parsed   (parse (remove-if #'not (tokenize-line ci))))
          (enqueue q (. (. (copying-stream-recorded-in ci)
                           (. file-id line-nr))
                        parsed)))))))

(defun parse-string (source)
  (with-stream-string in source (parse-stream in)))

(defun parse-files (filepaths)
  (apply #'+ (filter [(format t "Parsing '~A'…~%" _)
                      (with-input-file i _
                        (parse-stream i))]
                     filepaths)))
