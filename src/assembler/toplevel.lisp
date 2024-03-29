;;; bender
;;; Copyright (c) 2014–2015,2021,2024 Sven Michael Klose <pixel@copei.de>

(fn assemble-parsed-files (x)
  (!= (assemble-multiple-passes x)
    (unless *unassigned-segment-blocks*
      (return !)))
  (format t "Assembling again to assign BLOCKs to SEGMENTS…~%")
  (sort-unassigned-segment-blocks)
  (with-temporary *assign-blocks-to-segments?* t
    (assemble-multiple-passes x
        :unassigned-segment-blocks *unassigned-segment-blocks*
        :segments *segments*
        :description " (assigning BLOCKs to SEGMENTs)")))

(fn assemble-files (out-name &rest in-names)
  (with-temporary *unassigned-segment-blocks* nil
    (let dump-name (+ out-name ".lbl")
      (format t "Assembling to '~A'. Dump file is '~A'…~%" out-name dump-name)
      (with-output-file out out-name
        (with-output-file dump dump-name
          (with-temporary *assembler-dump-stream* dump
          (write-assembled-expressions
              (prog1 (assemble-parsed-files (parse-files in-names))
                (check-on-unassigned-blocks)
                (rewind-labels)
                (= *pc* 0))
              out))))))
  nil)
