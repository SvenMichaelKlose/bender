; bender – Copyright (c) 2015 Sven Michael Klose <pixel@hugbox.org>

(defun minigrafik-without-code (pathname)
  (subseq (fetch-file pathname) 15 (+ 3840 120 17)))
