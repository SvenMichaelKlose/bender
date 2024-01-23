; bender – Copyright (c) 2014–2015,2024 Sven Michael Klose <pixel@copei.de>

(fn assemble-org (x)
  (= *pc* (assemble-expression ..x.))
  nil)

(fn assemble-fill (x)
  (when (< 1 *pass*)
    (!= (assemble-expression ..x.)
      (& (< ! 0)
         (assembler-error "Cannot fill minus ~A bytes." (abs !)))
      (maptimes [identity 0] !))))

(fn make-returner ()
  (with (disabled?  *disabled?*
         data?      *data?*)
    [= *disabled?* disabled?
       *data?*     data?]))

(fn push-sourceblock (name)
  (push (make-sourceblock :name     name
                          :returner (make-returner))
        *sourceblock-stack*))

(fn assemble-if (x)
  (| ..x
     (assembler-error "IF expects a Lisp expression."))
  (push-sourceblock 'if)
  (= *disabled?* (not (assemble-expression ..x. :ensure? t :not-zero? t)))
  nil)

(fn assemble-data (x)
  (push-sourceblock 'data)
  (= *data?* t)
  nil)

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

(fn assemble-end (x)
  ; TODO a proper parser would do wonders here.
  (| *sourceblock-stack*
     (assembler-error "Unexpected END. No block open."))
  (& ...x
     (assembler-error (+ "END doesn't expect more than one optional argument."
                         " (IF, DATA or BLOCK.)")))
  (| (in? (cdr ..x.) nil 'if 'data 'block)
     (assembler-error "END expects IF, DATA or BLOCK as an argument."))
  (with (b         (pop *sourceblock-stack*)
         name      (cdr ..x.)
         expected  (sourceblock-name b))
    (!? name
        (| (eq ! expected)
           (assembler-error "Unexpected END of ~A. Expected ~A." ! expected)))
    (= (sourceblock-pc-end b) *pc*)
    (funcall (sourceblock-returner b) b))
  nil)

(fn assemble-directive (x)
  (case .x.
    'org    (assemble-org x)
    'fill   (assemble-fill x)
    'if     (assemble-if x)
    'data   (assemble-data x)
    'block  (assemble-block x)
    'end    (assemble-end x)
    (assembler-error "Unsupported directive ~A." x)))
