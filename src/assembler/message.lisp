; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun assembler-error (x &rest fmt)
  (alet *assembler-current-line*.
    (error (+ (when (cadr !)
                (format nil "~LError while assembling '~A', line ~A:~%~A"
                            (cadr !) (cddr !) !.))
              "~A")
           (apply #'format nil x fmt))))

(defun assembler-hint (x &rest fmt)
  (when (< 2 *pass*)
    (alet *assembler-current-line*.
      (when (cadr !)
        (format t (+ (format nil "~LHint for '~A', line ~A:~%~A"
                                 (cadr !) (cddr !) !.)
                     "~A~%")
                  (apply #'format nil x fmt))))))
