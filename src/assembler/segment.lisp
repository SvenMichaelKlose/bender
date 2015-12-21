; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defvar *unassigned-segment-blocks* nil)
(defvar *segment-mode* nil)

(defun fill-up-remaining-segment (bytes-left)
  (format t "~LFilling up remaining segment space with ~A zeroes.~%" bytes-left)
  (list (. *assembler-current-line*.
           (maptimes [identity 0] bytes-left))))

(defun assign-segment-block (bytes-left b may-be-shorter?)
  (= *unassigned-segment-blocks* (remove b *unassigned-segment-blocks* :test #'eq))
  (format t " ~A~F" (sourceblock-size b))
  (+ (butlast (queue-list (sourceblock-exprs b)))
     (try-to-assign-segment-block (- bytes-left (sourceblock-size b))
                                  may-be-shorter?)))

(defun find-largest-fitting-block (bytes-left)
  (find-if [<= (sourceblock-size _) bytes-left] *unassigned-segment-blocks*))

(defun try-to-assign-segment-block (bytes-left may-be-shorter?)
  (unless (zero? bytes-left)
    (!? (find-largest-fitting-block bytes-left)
        (assign-segment-block bytes-left ! may-be-shorter?)
        (? may-be-shorter?
           (format t "~LTrimmed segment by ~A bytes.~%" bytes-left)
           (fill-up-remaining-segment bytes-left)))))

(defun fill-segment (size may-be-shorter?)
  (& (zero? size)
     (assembler-error "SEGMENT size must be larger than 0."))
  (format t "Filling up segment of size ~a…" size)
  (prog1 (try-to-assign-segment-block size may-be-shorter?)
    (fresh-line)))

(defun segment (&key size (may-be-shorter? nil))
  (| (number? size)
     (assembler-error "SEGMENT expects an integer size."))
  (?
    (& (eq *segment-mode* :assign)
       may-be-shorter?)         (fill-segment size may-be-shorter?)
    (& (eq *segment-mode* :assign-fixed)
       (not may-be-shorter?))   (fill-segment size may-be-shorter?)))

(defun sort-unassigned-segment-blocks ()
  (= *unassigned-segment-blocks* (sort *unassigned-segment-blocks*
                                       :test #'((a b)
                                                 (>= (sourceblock-size a)
                                                     (sourceblock-size b))))))

(defun size-of-unassigned-blocks ()
  (apply #'+ (@ #'sourceblock-size *unassigned-segment-blocks*)))

(defun check-on-unassigned-blocks ()
  (awhen *unassigned-segment-blocks*
    (assembler-error (+ "~A BLOCKs of ~A bytes in total couldn't get assigned to SEGMENTs ~%"
                        "Remaining blocks: ~A")
                     (length *unassigned-segment-blocks*)
                     (size-of-unassigned-blocks)
                     *unassigned-segment-blocks*)))

(defun assign-blocks-to-segments (x)
  (with-temporary *segment-mode* :assign-fixed
    (assemble-pass x :description " (assigning BLOCKs to fixed-sized SEGMENTs)"))
  (with-temporary *segment-mode* :assign
    (assemble-pass x :description " (assigning BLOCKs to fixed-sized SEGMENTs)"))
  (check-on-unassigned-blocks)
  (with-temporary *segment-mode* :emit
    (assemble-multiple-passes x)))

(defun assemble-segments (x)
  (sort-unassigned-segment-blocks)
  (with-temporary *unassigned-segment-blocks* nil
    (with-temporary *segment-mode* :collect
      (assemble-pass x :description " (collecting SEGMENT BLOCKs)"))
    (? *unassigned-segment-blocks*
       (assign-blocks-to-segments x)
       x)))
