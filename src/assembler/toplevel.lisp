; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun assemble-parsed-files (x)
  (alet (assemble-multiple-passes x)
    (unless *unassigned-segment-blocks*
      (return !)))
  (format t "Assembling again to assign BLOCKs to SEGMENTS…~%")
  (sort-unassigned-segment-blocks)
  (with-temporary *assign-blocks-to-segments?* t
    (assemble-multiple-passes x :unassigned-segment-blocks *unassigned-segment-blocks*
                                :segments *segments*
                                :description " (assigning BLOCKs to SEGMENTs)")))

(defun assemble-files (out-name &rest in-names)
  (with-temporary *unassigned-segment-blocks* nil
    (let dump-name (+ out-name ".lst")
      (format t "Assembling to '~A'. Dump file is '~A'…~%" out-name dump-name)
      (with-output-file out out-name
        (write-assembled-expressions (prog1 (assemble-parsed-files (parse-files in-names))
                                       (check-on-unassigned-blocks)
                                       (rewind-labels))
                                     out))))
  nil)
