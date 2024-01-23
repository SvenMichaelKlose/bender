; bender – Copyright (c) 2014–2015,2024 Sven Michael Klose <pixel@copei.de>

(defstruct sourceblock
  name
  returner
  (exprs (make-queue))
  pc-start
  pc-end)

(def-sourceblock sourceblock-size (sourceblock)
  (- pc-end pc-start))

(var *sourceblock-stack* nil)
(var *unassigned-segment-blocks* nil)
(var *assign-blocks-to-segments?* nil)

(fn make-block-returner ()
  (!= (make-returner)
    [(| *assign-blocks-to-segments?*
        (push _ *unassigned-segment-blocks*))
     (funcall ! _)]))

(fn assemble-block (x)
  (push (make-sourceblock :name 'block
                          :returner (make-block-returner)
                          :pc-start *pc*)
        *sourceblock-stack*)
  (= *disabled?* *assign-blocks-to-segments?*)
  nil)

(fn fill-up-remaining-segment (bytes-left)
  (format t "~LFilling up remaining segment space with ~A zeroes.~%"
            bytes-left)
  (list (. *assembler-current-line*.
           (maptimes [identity 0] bytes-left))))

(fn assign-segment-block (bytes-left b may-be-shorter?)
  (= *unassigned-segment-blocks*
     (remove b *unassigned-segment-blocks* :test #'eq))
  (format t " ~A~F" (sourceblock-size b))
  (+ (butlast (queue-list (sourceblock-exprs b)))
     (try-to-assign-segment-block (- bytes-left (sourceblock-size b))
                                  may-be-shorter?)))

(fn find-largest-fitting-block (bytes-left)
  (find-if [<= (sourceblock-size _) bytes-left]
           *unassigned-segment-blocks*))

(fn try-to-assign-segment-block (bytes-left may-be-shorter?)
  (unless (== 0 bytes-left)
    (!? (find-largest-fitting-block bytes-left)
        (assign-segment-block bytes-left ! may-be-shorter?)
        (? may-be-shorter?
           (format t "~LTrimmed segment by ~A bytes.~%" bytes-left)
           (fill-up-remaining-segment bytes-left)))))

(fn fill-segment (size may-be-shorter?)
  (& (== 0 size)
     (assembler-error "SEGMENT size must be larger than 0."))
  (format t "Filling up segment of size ~a…" size)
  (prog1 (try-to-assign-segment-block size may-be-shorter?)
    (fresh-line)))

(fn segment (&key size (may-be-shorter? nil))
  (| (number? size)
     (assembler-error "SEGMENT expects a size."))
  (? *assign-blocks-to-segments?*
     (fill-segment size may-be-shorter?)))

(fn sort-unassigned-segment-blocks ()
  (= *unassigned-segment-blocks*
     (sort *unassigned-segment-blocks*
           :test #'((a b)
                     (>= (sourceblock-size a)
                         (sourceblock-size b))))))

(fn check-on-unassigned-blocks ()
  (awhen *unassigned-segment-blocks*
    (assembler-error (+ "~A BLOCKs couldn't get assigned to SEGMENTs"
                        " (~A bytes). Remaining blocks: ~A")
                     (length *unassigned-segment-blocks*)
                     (apply #'+ (@ #'sourceblock-size
                                   *unassigned-segment-blocks*))
                     *unassigned-segment-blocks*)))
