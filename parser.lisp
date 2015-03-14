; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun parse-labels (x)
  (& x
     (? (& (eq 'identifier (car x.))
           (eq 'colon (car .x.)))
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
         (? (eq 'comma (car .x.))
            (? (eq 'identifier (car ..x.))
               (list (?
                       (string== "X" (upcase (cdr ..x.)))  'absx
                       (string== "Y" (upcase (cdr ..x.)))  'absy
                       (error "Index register expected instead of '~A'." (cdr ..x.)))
                     x.)
               (error "Index register expected."))
            (error "Comma expected instead of ~A." (car .x.))))
    (eq 'bracket-open x..)
      (progn
        (| (number-or-identifier? .x.)
           (error "Expression expected instead of ~A." (car .x.)))
        (? (eq 'bracket-close (car ..x.))
           (?
             (eq 'comma (car ...x.))
               (?
                 (string== "Y" (upcase (cdr ....x.)))  (list 'izpy .x.)
                 (error "Index register Y expected."))
             ...x.
               (error "Comma or end of line expected instead of ~A." (car ...x.))
             (list 'indi .x.))
           (eq 'comma (car ..x.))
             (? (eq 'identifier (car ...x.))
                (?
                  (string== "X" (upcase (cdr ...x.)))
                    (? (eq 'bracket-close (car ....x.))
                       (list 'izpx .x.)
                       (error "Closing bracket expected instead of ~A." ....x.))
                  (error "Index register X expected."))
                (error "Index register X expected."))
           (error "~A closing bracket."
                  (? (member-if [eq 'bracket-close _.] ...x)
                     "Misplaced"
                     "Missing"))))
    (error "Syntax error at ~A." x.)))

(defun parse-mnemonic (x)
  (parse-addrmode x))

(defun parse-assignment (x)
  (| (eq 'assignment (car .x.))
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
        'mnemonic    `((,(car x.) ,(cdr x.) ,@(parse-mnemonic .x)))
        (? (eq 'identifier (car x.))
           (? (eq 'assignment (car .x.))
              (parse-assignment x)
              (. x. (parse-0 .x)))
           (error "Unexpected token ~A." x.))))))

(defun parse (x)
  (awhen (parse-labels x)
    (parse-0 !)))

(defun parse-stream (i)
  (with-queue q
    (while (peek-char i)
           (apply #'+ (queue-list q))
      (with-stream-string line (princ (read-line i))
        (!? (late-print (parse (late-print (remove-if #'not (tokenize-line line)))))
            (enqueue q !))))))
