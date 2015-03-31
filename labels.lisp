; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defvar *previous-labels* nil)
(defvar *next-labels*     nil)
(defvar *label-changed?* nil)

(defun clear-labels ()
  (= *previous-labels* nil)
  (= *next-labels*     nil)
  (= *label-changed?*  nil))

(defun rewind-labels ()
  (= *next-labels* (reverse *previous-labels*)))

(defun update-label (x addr)
  (alet *next-labels*.
    (unless (eq !. x)
      (error "Internal error: wanted to update label ~A but found ~A." x !.))
    (unless (== .! addr)
      (= .! addr)
      (= *label-changed?* t)))
  (push (pop *next-labels*) *previous-labels*))

(defun add-label (x addr)
  (? (first-pass?)
     (acons! x addr *previous-labels*)
     (update-label x addr)))

(defun get-label-in (ltab x direction required?)
  (| (cdr (assoc x ltab :test #'string==))
     (& required?
        (? (first-pass?)
           0
           (error "No ~A label '~A'." direction x)))))

(defun get-previous-label (x &key (required? t))
  (get-label-in *previous-labels* x "previous" required?))

(defun get-next-label (x &key (required? t))
  (get-label-in *next-labels* x "next" required?))

(defun get-label-undirected (x)
  (with (prev  (get-previous-label x :required? nil)
         next  (get-next-label     x :required? nil))
    (& prev next
       (error "Label ~A appears in previous and later code. Please specify a direction." x))
    (| prev next
       (error "Label ~A is not defined." x))))

(defun get-label (x)
  (alet (string-list x)
    (case !. :test #'==
      #\-  (get-previous-label (list-string .!))
      #\+  (get-next-label (list-string .!))
      (get-label-undirected x))))
