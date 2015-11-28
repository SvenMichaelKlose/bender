; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defvar *assembler-current-line* nil)
(defvar *pc* nil)
(defvar *pass* nil)
(defvar *disabled?* nil)
(defvar *data?* nil)
(defvar *cycles* nil)
(defvar *acycles* 0)

(defun first-pass? ()
  (< *pass* 1))
