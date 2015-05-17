; bender – Copyright (c) 2015 Sven Michael Klose <pixel@hugbox.org>

(defvar *model* nil)
(defvar *basic-start* nil)

(defun basic-start-address (model)
  (case model
    nil         *basic-start*
    :c64        #x0801
    :vic-20     #x1001
    :vic-20+3k  #x0401
    :vic-20+xk  #x1201
    (error "Cannot determine BASIC start address for model ~A." model)))
