(defun exomize (from to addr target &key (path "/usr/local/bin/exomizer"))
  (sb-ext:run-program path
                      `("sfx" ,(+ "$" addr)
                        "-t" ,target
                        "-n"
                        ,(+ "-Di_load_addr=$" addr)
                        "-o" ,to
                        ,from)
                      :pty cl:*standard-output*))
