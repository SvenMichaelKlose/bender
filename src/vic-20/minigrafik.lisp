; bender – Copyright (c) 2015,2024 Sven Michael Klose <pixel@hugbox.org>

(fn minigrafik-without-code (pathname)
  (subseq (fetch-file pathname) 15 (+ 3840 120 17)))
