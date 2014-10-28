;;;;; bender – Copyright (c) 2014 Sven Michael Klose <pixel@copei.de>

(defvar *labels*)
(defvar *scope*)
(defvar *scopes*)
(defvar *current-scope*)
(defvar *label-changed?*)

(defun clear-labels ()
  (= *labels* (make-hash-table :test #'string==))
  (= *scope* nil)
  (= *scopes* nil)
  (= *current-scope* nil))

(defun make-scope ()
  (make-hash-table :test #'string==))

(defun open-scope ()
  (? (zero? *pass*)
     (= *current-scope* (car (push (make-scope) *scopes*)))
     (= *current-scope* *scope*.))
  (| *current-scope*
     (error "Scope unset.")))

(defun close-scope ()
  (= *current-scope* nil)
  (= *scope* .*scope*))

(defun add-label (x addr)
  (let s (| *current-scope* *labels*)
    (alet (string-downcase x)
      (& (zero? *pass*)
         (href s !)
         (error "Label '~A' is already defined." !))
      (? (not (equal (href s !) addr))
         (= *label-changed?* t))
      (= (href s !) addr))))

(defun get-label-0 (s x)
  (& s (href s (string-downcase x))))

(defun get-label (x)
  (| (get-label-0 *current-scope* x)
     (get-label-0 *labels* x)
     (? (zero? *pass*)
        0
        (error "Label '~A' is not defined." x))))
