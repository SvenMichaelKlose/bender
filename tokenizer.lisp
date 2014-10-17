;;;;; bender – Copyright (c) 2014 Sven Michael Klose <pixel@copei.de>

(defvar *char-tokens*
    '((#\# . hash)
      (#\, . comma)
      (#\( . bracket-open)
      (#\) . bracket-close)
      (#\: . colon)
      (#\= . assignment)))

(defconstant *directives* '(org open_scope close_scope fill))

(defun skip-whitespaces (in)
  (alet (peek-char in)
    (unless (end-of-file? in)
      (when (| (control-char? !)
            (== #\  !))
        (read-char in)
        (skip-whitespaces in)))))

(defun read-identifier (in)
  (alet (peek-char in)
    (unless (end-of-file? in)
       (& (| (alpha-char? !)
             (digit-char? !)
             (== #\_ !))
          (. (read-char in)
             (read-identifier in))))))

(defun directive? (x)
  (member x *directives* :test #'eq))

(defun mnemonic? (x)
  (member x *mnemonic-list* :test #'eq))

(defun tokenize-identifier (in)
  (awhen (read-identifier in)
    (let n (list-string !)
      (alet (make-symbol (string-upcase n))
        (?
          (directive? !) (. 'directive !)
          (mnemonic? !)  (. 'mnemonic !)
          (. 'identifier n))))))

(defun tokenize-decimal (in)
  (unless (end-of-file? in)
    (. 'number (read-number in))))

(defun tokenize-hexadecimal (in)
  (read-char in)
  (. 'number (read-hex in)))

(defun tokenize-binary (in)
  (read-char in)
  (. 'number (read-binary in)))

(defun tokenize-expression (in)
  (read-char in)
  (. 'expression (read in)))

(defun tokenize-string (in)
  (read-char in)
  (. 'string (read-string in)))

(defun tokenize (in)
  (skip-whitespaces in)
  (alet (peek-char in)
    (let ct (member-if [== ! _.] *char-tokens*)
      (? ct
         (progn
           (read-char in)
           (list (cdr ct.)))
         (?
           (== ! #\")       (tokenize-string in)
           (== ! #\@)       (tokenize-expression in)
           (== ! #\$)       (tokenize-hexadecimal in)
           (== ! #\%)       (tokenize-binary in)
           (== ! #\;)       (progn
                              (read-line in)
                              nil)
           (digit-char? !)  (tokenize-decimal in)
           (| (tokenize-identifier in)
              (? (control-char? !)
                 (read-char in)
                 (error "Unexpected character ~A." (read-char in)))))))))

(defun tokenize-line (in)
  (peek-char in)
  (unless (end-of-file? in)
    (. (tokenize in)
       (tokenize-line in))))
