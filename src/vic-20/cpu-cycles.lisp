; Bender – Copyright (c) 2015,2024 Sven Michael Klose <pixel@hugbox.org>

(defconstant +cpu-cycles-pal+ 1108404)
(defconstant +cpu-cycles-ntsc+ 1027270)

(fn cpu-cycles (tv)
  (case tv
    :ntsc   +cpu-cycles-ntsc+
    :pal    +cpu-cycles-pal+
    (error ":NTSC or :PAL expected.")))
