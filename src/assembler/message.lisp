; bender – Copyright (c) 2014–2015,2024 Sven Michael Klose <pixel@copei.de>

(fn assembler-error (x &rest fmt)
  (!= *assembler-current-line*.
    (error (+ (when .!.
                (format nil "~LError while assembling '~A', line ~A:~%~A"
                            .!. ..! !.))
              "~A")
           (apply #'format nil x fmt))))

(fn assembler-hint (x &rest fmt)
  (when (< 2 *pass*)
    (!= *assembler-current-line*.
      (when (cadr !)
        (format t (+ (format nil "~LHint for '~A', line ~A:~%~A" .!. ..! !.)
                     "~A~%")
                  (apply #'format nil x fmt))))))
