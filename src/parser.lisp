; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(def-head-predicate identifier)
(def-head-predicate colon)
(def-head-predicate hash)
(def-head-predicate comma)
(def-head-predicate bracket-open)
(def-head-predicate bracket-close)
(def-head-predicate assignment)
(def-head-predicate label)
(def-head-predicate instruction)

(defun parse-labels (x)
  (& x
     (? (& (identifier? x.)
           (colon? .x.))
        (. (. 'label (cdr x.))
           (parse-labels ..x))
        (. x. (parse-labels .x)))))

(defun number-or-identifier? (x)
  (in? x. 'identifier 'number 'expression))

(defun parse-addrmode (x)
  (?
    (not x)
      (list 'accu nil)
    (eq 'hash x..)
      (progn
        (| (number-or-identifier? .x.)
           (error "Expression expected instead of ~A." (car .x.)))
        (list 'imm .x.))
    (number-or-identifier? x.)
      (? (not .x)
         (list 'abs x.)
         (? (comma? .x.)
            (? (identifier? ..x.)
               (list (case (cdr ..x.)
                       'x  'absx
                       'y  'absy
                       (error "Index register expected instead of '~A'." (cdr ..x.)))
                     x.)
               (error "Index register expected."))
            (error "Comma expected instead of ~A." (car .x.))))
    (bracket-open? x.)
      (progn
        (| (number-or-identifier? .x.)
           (error "Expression expected instead of ~A." (car .x.)))
        (? (bracket-close? ..x.)
           (?
             (comma? ...x.)
               (?
                 (eq 'y (cdr ....x.))  (list 'izpy .x.)
                 (error "Index register Y expected."))
             ...x.
               (error "Comma or end of line expected instead of ~A." (car ...x.))
             (list 'indi .x.))
           (comma? ..x.)
             (? (identifier? ...x.)
                (?
                  (eq 'x (cdr ...x.))
                    (? (bracket-close? ....x.)
                       (list 'izpx .x.)
                       (error "Closing bracket expected instead of ~A." ....x.))
                  (error "Index register X expected."))
                (error "Index register X expected."))
           (error "~A closing bracket."
                  (? (member-if #'bracket-close? ...x)
                     "Misplaced"
                     "Missing"))))
    (error "Syntax error at ~A." x.)))

(defun parse-assignment (x)
  (| (assignment? .x.)
     (error "Assignment '=' expected instead of ~A." .x))
  `((assignment ,(cdr x.) ,..x.)))

(defun parse-directive (x)
  `((,(car x.) ,(cdr x.) ,@.x)))

(defun parse-0 (x)
  (when x
    (?
      (not x.)  (parse-0 .x)
      (in? x.. 'number 'string 'expression) (. x. (parse-0 .x))
      (case x.. :test #'eq
        'label       (. x. (parse-0 .x))
        'directive   (parse-directive x)
        'mnemonic    `((instruction ,(cdr x.) ,@(parse-addrmode .x)))
        (? (identifier? x.)
           (? (assignment? .x.)
              (parse-assignment x)
              (. x. (parse-0 .x)))
           (error "Unexpected token ~A." x.))))))

(defun parse (x)
  (awhen (parse-labels x)
    (with (f [? (& (cons? _)
                   (in? _. 'number 'string)) ._
                (cons? _)  (. (f _.) (f ._))
                _])
      (f (parse-0 !)))))

(defun parse-stream (i)
  (with-queue q
    (while (peek-char i)
           (queue-list q)
      (let ci (make-copying-stream :in i)
        (let-when parsed (parse (remove-if #'not (tokenize-line ci)))
          (enqueue q (. (copying-stream-recorded-in ci)
                        parsed)))))))

(defun parse-string (source)
  (with-stream-string in source (parse-stream in)))

(defun parse-files (filepaths)
  (apply #'+ (filter [(format t "Parsing '~A'…~%" _)
                      (with-input-file i _
                        (parse-stream i))]
                     filepaths)))
