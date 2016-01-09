; bender – Copyright (c) 2015–2016 Sven Michael Klose <pixel@hugbox.org>

(defun exomize (from to addr target)
  (sb-ext:run-program "/usr/local/bin/exomizer"
                      `("sfx" ,(+ "$" addr)
                        "-t" ,target
                        "-n"
                        ,(+ "-Di_load_addr=$" addr)
                        "-o" ,to
                        ,from)
                      :pty cl:*standard-output*))
