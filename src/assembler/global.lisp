; bender – Copyright (c) 2014–2015,2024 Sven Michael Klose <pixel@copei.de>

(var *assembler-current-line* nil)
(var *pc* nil)
(var *pass* nil)
(var *disabled?* nil)
(var *data?* nil)
(var *cycles* nil)
(var *acycles* 0)

(fn first-pass? ()
  (< *pass* 1))
