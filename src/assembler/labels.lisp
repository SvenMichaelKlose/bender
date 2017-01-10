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
      (assembler-error (+ "Wanted to update label ~A but found ~A.~%"
                          "Please make sure that your code does not change across passes.")
                       x !.))
    (unless (== .! addr)
      (= .! addr)
      (= *label-changed?* t)))
  (push (pop *next-labels*) *previous-labels*))

(defun add-label (x addr)
  (? (first-pass?)
     (acons! x addr *previous-labels*)
     (update-label x addr)))

(defun get-label-in (ltab x direction required?)
  (| (cdr (assoc x ltab :test #'eq))
     (when required?
       (assembler-error "No ~A label '~A'." direction x))))

(defun get-earlier-label (x &key (required? t))
  (get-label-in *previous-labels* x "previous" required?))

(defun get-later-label (x &key (required? t))
  (get-label-in *next-labels* x "next" required?))

(defun has-label? (x)
  (| (cdr (assoc x *previous-labels* :test #'eq))
     (cdr (assoc x *next-labels* :test #'eq))))

(defun get-label-undirected (x &key (required? t))
  (with (prev  (get-earlier-label x :required? nil)
         next  (get-later-label x :required? nil))
    (& required? prev next
       (assembler-error (+ "Label ~A appears in earlier and later code.~%"
                           "Please specify a direction by prependig a `+` or `-`.~%"
                           "If you want to look up the label from inside a Lisp expression,~%"
                           "please see functions GET-EARLIER-LABEL and GET-LATER-LABEL.~%")
                        x))
    (| prev next
       (cdr (assoc x *imported-labels* :test #'eq))
       (& required?
          (assembler-error "Label ~A is not defined." x)))))

(defun get-label (x &key (required? t))
  (when x
    (let n (symbol-name x)
      (alet (& (< 1 (length n)) 
               (make-symbol (subseq n 1)))
        (case (elt n 0) :test #'character==
          #\-  (get-earlier-label ! :required? required?)
          #\+  (get-later-label ! :required? required?)
          #\<  (low (| (get-label ! :required? required?) 0))
          #\>  (high (| (get-label ! :required? required?) 0))
          (get-label-undirected x :required? required?))))))

(defun get-labels ()
  *next-labels*)
