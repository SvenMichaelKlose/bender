; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defvar *previous-labels* nil)
(defvar *next-labels*     nil)
(defvar *imported-labels* nil)
(defvar *label-changed?* nil)

(defun clear-labels ()
  (= *previous-labels* nil)
  (= *next-labels*     nil)
  (= *label-changed?*  nil))

(defun rewind-labels ()
  (= *next-labels* (reverse *previous-labels*))
  (= *previous-labels* nil))

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
  (| (cdr (assoc x ltab))
     (when required?
       (error "No ~A label '~A'." direction x))))

(defun get-previous-label (x &key (required? t))
  (get-label-in *previous-labels* x "previous" required?))

(defun get-next-label (x &key (required? t))
  (get-label-in *next-labels* x "next" required?))

(defun get-label-undirected (x &key (required? t))
  (with (prev  (get-previous-label x :required? nil)
         next  (get-next-label x :required? nil))
    (& required? prev next
       (error "Label ~A appears in previous and later code. Please specify a direction." x))
    (| prev next
       (cdr (assoc x *imported-labels*))
       (& required?
          (error "Label ~A is not defined." x)))))

(defun get-label (x &key (required? t))
  (when x
    (let n (symbol-name x)
      (alet (& (< 1 (length n)) 
               (make-symbol (subseq n 1)))
        (case (elt n 0) :test #'==
          #\-  (get-previous-label ! :required? required?)
          #\+  (get-next-label ! :required? required?)
          #\<  (low (| (get-label ! :required? required?) 0))
          #\>  (high (| (get-label ! :required? required?) 0))
          (get-label-undirected x :required? required?))))))

(defun get-labels ()
  *next-labels*)
