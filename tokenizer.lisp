; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defconstant +char-tokens+
    '((#\# . hash)
      (#\, . comma)
      (#\( . bracket-open)
      (#\) . bracket-close)
      (#\: . colon)
      (#\= . assignment)))

(defconstant +extra-identifier-chars+ '(#\_ #\+ #\-))

(defun extra-identifier-char? (x)
  (member x +extra-identifier-chars+ :test #'character==))

(defconstant +directives+ '(org fill))

(defun skip-whitespaces (in)
  (awhen (peek-char in)
    (when (| (control-char? !)
             (== #\  !))
      (read-char in)
      (skip-whitespaces in))))

(defun read-identifier (in)
  (awhen (peek-char in)
    (& (| (alpha-char? !)
          (digit-char? !)
          (extra-identifier-char? !))
       (. (read-char in)
          (read-identifier in)))))

(defun directive? (x)
  (member x +directives+ :test #'eq))

(defun mnemonic? (x)
  (member x *mnemonic-list* :test #'eq))

(defun tokenize-identifier (in)
  (awhen (make-symbol (upcase (list-string (read-identifier in))))
    (. (?
         (directive? !)  'directive
         (mnemonic? !)   'mnemonic
         'identifier)
       !)))

(defun tokenize-decimal (in)
  (when (peek-char in)
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
  (awhen (peek-char in)
    (let ct (member-if [== ! _.] +char-tokens+)
      (? ct
         (progn
           (read-char in)
           (list (cdr ct.)))
         (?
           (== ! #\")       (tokenize-string in)
           (== ! #\@)       (tokenize-expression in)
           (== ! #\$)       (tokenize-hexadecimal in)
           (== ! #\%)       (tokenize-binary in)
           (== ! #\;)       (& (read-line in) nil)
           (digit-char? !)  (tokenize-decimal in)
           (| (tokenize-identifier in)
              (? (control-char? !)
                 (read-char in)
                 (error "Unexpected character ~A." (read-char in)))))))))

(defun tokenize-line (in)
  (when (peek-char in)
    (. (tokenize in)
       (tokenize-line in))))
