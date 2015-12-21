; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defstruct sourceblock
  name
  returner
  (exprs (make-queue))
  pc-start
  pc-end)

(def-sourceblock sourceblock-size (sourceblock)
  (- pc-end pc-start))

(defvar *sourceblock-stack* nil)

(defun make-returner ()
  (with (disabled?  *disabled?*
         data?      *data?*)
    [= *disabled?* disabled?
       *data?*     data?]))

(defun push-sourceblock (name)
  (push (make-sourceblock :name name
                          :returner (make-returner))
        *sourceblock-stack*))
