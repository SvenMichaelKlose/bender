(defconstant +char-tokens+
    '((#\# . hash)
      (#\, . comma)
      (#\( . bracket-open)
      (#\) . bracket-close)
      (#\: . colon)
      (#\= . assignment)))

(fn char-token (x)
  (cdar (member-if [character== x _.] +char-tokens+)))

(defconstant +extra-identifier-chars+ '(#\_ #\+ #\- #\< #\>))

(fn extra-identifier-char? (x)
  (member x +extra-identifier-chars+ :test #'character==))

(defconstant +directives+ '(org fill if data block end))

(fn skip-whitespaces (in)
  (awhen (peek-char in)
    (unless (== 10 (char-code !))
      (when (| (control-char? !)
               (character== #\  !))
        (read-char in)
        (skip-whitespaces in)))))

(fn read-identifier (in)
  (awhen (peek-char in)
    (& (| (alpha-char? !)
          (digit? !)
          (extra-identifier-char? !))
       (. (read-char in)
          (read-identifier in)))))

(fn directive? (x)
  (member x +directives+ :test #'eq))

(fn mnemonic? (x)
  (member (case x :test #'eq
            'blt 'bcc
            'bge 'bcs
            x)
          *mnemonic-list* :test #'eq))

(fn tokenize-identifier (in)
  (!? (read-identifier in)
      (!= (make-symbol (upcase (list-string !)))
        (. (?
             (directive? !)  'directive
             (mnemonic? !)   'mnemonic
             'identifier)
           !))))

(fn tokenize-decimal (in)
  (& (peek-char in)
     (read-number in)))

(fn tokenize-hexadecimal (in)
  (read-char in)
  (read-hex in))

(fn tokenize-binary (in)
  (read-char in)
  (read-binary in))

(fn tokenize-expression (in)
  (read-char in)
  (. 'expression (read in)))

(fn tokenize-string (in)
  (read-char in)
  (read-string in))

(fn tokenize-comment (in)
  (awhile (not (character== (code-char 10) (peek-char in)))
          nil
    (read-char in)))

(fn tokenize (in)
  (skip-whitespaces in)
  (awhen (peek-char in)
    (? (char-token !)
       (list (char-token (read-char in)))
       (case ! :test #'character==
         #\"  (tokenize-string in)
         #\@  (tokenize-expression in)
         #\$  (tokenize-hexadecimal in)
         #\%  (tokenize-binary in)
         #\;  (tokenize-comment in)
         (? (digit? !)
            (tokenize-decimal in)
            (| (tokenize-identifier in)
               (? (control-char? !)
                  (& (read-char in) nil)
                  (error "Unexpected character ~A." (read-char in)))))))))

(fn tokenize-line (in)
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
