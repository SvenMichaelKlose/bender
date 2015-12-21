; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun assemble-pass (x &key description)
  (format t "Pass ~A~A…~%" *pass* description)
  (= *label-changed?* nil
     *pc*       0
     *acycles*  0)
  (rewind-labels)
  (aprog1 (assemble-parsed-expressions x)
    (++! *pass*)))

(defun assemble-multiple-passes (x &key (description ""))
  (clear-labels)
  (= *pass* 0)
  (prog1 (while (| *label-changed?*
                   (< *pass* 4))
                (assemble-pass x :description description)
           (assemble-pass x :description description))
    (!? *sourceblock-stack*
        (assembler-error "Block(s) with no END: ~A" !))))
