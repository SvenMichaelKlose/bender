; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun assemble-parsed-files (x)
  (assemble-segments (assemble-multiple-passes x)))

(defun assemble-files (out-name &rest in-names)
  (with-temporary *unassigned-segment-blocks* nil
    (let dump-name (+ out-name ".lst")
      (format t "Assembling to '~A'. Dump file is '~A'…~%" out-name dump-name)
      (with-output-file out out-name
        (with-output-file dump dump-name
          (with-temporary *assembler-dump-stream* dump
          (write-assembled-expressions (assemble-parsed-files (parse-files in-names))
                                       out))))))
  (rewind-labels)
  nil)
