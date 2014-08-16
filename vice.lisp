;;;;; bender – Copyright (c) 2014 Sven Michael Klose <pixel@copei.de>

(defun make-vice-commands (out-name)
  (format t "Writing VICE monitor commands to '~A'.~%" out-name)
  (with-output-file o out-name
    (adolist ((hashkeys *labels*))
      (format o "al ~A .~A~%" (print-hexword (href *labels* !) nil) !))))
