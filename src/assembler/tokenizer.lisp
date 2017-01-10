(defconstant +char-tokens+
    '((#\# . hash)
      (#\, . comma)
      (#\( . bracket-open)
      (#\) . bracket-close)
      (#\: . colon)
      (#\= . assignment)))

(defun char-token (x)
  (cdar (member-if [character== x _.] +char-tokens+)))

(defconstant +extra-identifier-chars+ '(#\_ #\+ #\- #\< #\>))

(defun extra-identifier-char? (x)
  (member x +extra-identifier-chars+ :test #'character==))

(defconstant +directives+ '(org fill if data block end))

(defun skip-whitespaces (in)
  (awhen (peek-char in)
    (unless (== 10 (char-code !))
      (when (| (control-char? !)
               (character== #\  !))
        (read-char in)
        (skip-whitespaces in)))))

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
  (member (case x :test #'eq
            'blt 'bcc
            'bge 'bcs
            x)
          *mnemonic-list* :test #'eq))

(defun tokenize-identifier (in)
  (!? (read-identifier in)
      (alet (make-symbol (upcase (list-string !)))
        (. (?
             (directive? !)  'directive
             (mnemonic? !)   'mnemonic
             'identifier)
            !))))

(defun tokenize-decimal (in)
  (& (peek-char in)
     (read-number in)))

(defun tokenize-hexadecimal (in)
  (read-char in)
  (read-hex in))

(defun tokenize-binary (in)
  (read-char in)
  (read-binary in))

(defun tokenize-expression (in)
  (read-char in)
  (. 'expression (read in)))

(defun tokenize-string (in)
  (read-char in)
  (read-string in))

(defun tokenize-comment (in)
  (awhile (not (character== (code-char 10) (peek-char in)))
          nil
    (read-char in)))

(defun tokenize (in)
  (skip-whitespaces in)
  (awhen (peek-char in)
    (?
      (char-token !)   (list (char-token (read-char in)))
      (case ! :test #'character==
        #\"  (tokenize-string in)
        #\@  (tokenize-expression in)
        #\$  (tokenize-hexadecimal in)
        #\%  (tokenize-binary in)
        #\;  (tokenize-comment in)
        (? (digit-char? !)  (tokenize-decimal in)
           (| (tokenize-identifier in)
              (? (control-char? !)
                 (& (read-char in) nil)
                 (error "Unexpected character ~A." (read-char in)))))))))

(defun tokenize-line (in)
  (skip-whitespaces in)
  (awhen (peek-char in)
    (case ! :test #'character==
      (code-char 10)  (progn
                        (read-char in)
                        nil)
      (code-char 13)  (progn
                        (read-char in)
                        (tokenize-line in))
      (. (tokenize in)
         (tokenize-line in)))))
