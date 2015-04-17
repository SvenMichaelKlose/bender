; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun make-vice-commands (out-name)
  (format t "Writing VICE monitor commands to '~A'.~%" out-name)
  (with-output-file o out-name
    (format o "; For VICE with command line option -moncommands ~A" out-name)
    (adolist ((+ (reverse *previous-labels*) *next-labels*))
      (format o "al ~A .~A~%" (print-hexword .! nil) (downcase (symbol-name !.))))))
