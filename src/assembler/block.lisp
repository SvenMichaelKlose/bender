; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun make-block-returner ()
  (alet (make-returner)
    [(& (eq *segment-mode* :collect)
        (push _ *unassigned-segment-blocks*))
     (funcall ! _)]))

(define-directive block x
  (push (make-sourceblock :name 'block
                          :returner (make-block-returner)
                          :pc-start *pc*)
        *sourceblock-stack*)
  (= *disabled?* (in? *segment-mode* :assign :assign-fixed))
  nil)
