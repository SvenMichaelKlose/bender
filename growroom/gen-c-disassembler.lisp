; bender – Copyright (c) 2015 Sven Michael Klose <pixel@copei.de>

(defun gen-c-opcode (x)
  (+ "    "
     (? x.
        (format nil "\"~A\", am_~A"
                    (downcase (symbol-name x.))
                    (downcase (symbol-name .x)))
        (format nil "NULL, 0"))))

(defun gen-c-opcode-map ()
  (with-output-file o "obj/intruction-map.c"
    (format o "struct instruction {~%")
    (format o "    const char * mnemonic;~%")
    (format o "    char addrmode;~%")
    (format o "} opcode_map[256] = {~%")
    (princ (apply #'+ (pad (@ #'gen-c-opcode (array-list *opcode-map*))
                           (format nil ",~%")))
           o)
    (format o "~%}~%")))

(gen-c-opcode-map)
