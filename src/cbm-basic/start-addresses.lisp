; bender – Copyright (c) 2015–2016 Sven Michael Klose <pixel@hugbox.org>

(defvar *model* nil)

(defun basic-start-address (model)
  (case model
    :pet         #x0401
    :cbm         #x0401
    :vic-20      #x1001
    :vic-20+3k   #x0401
    :vic-20+xk   #x1201
    :c64         #x0801
    :plus4       #x1001
    :plus4hires  #x2001
    :c128        #x1c01
    :c128hires   #x4001
    (error "Cannot determine BASIC start address for model ~A." model)))
