; Bender – Copyright (c) 2015 Sven Michael Klose <pixel@hugbox.org>

(defun instruction-cycles (inst)
  (with (mnemonic  (instruction-mnemonic inst)
         addrmode  (instruction-addrmode inst))
    (| (assoc-value mnemonic +instruction-cycles+)
       (& (member mnemonic +rw-instructions+)
          (assoc-value addrmode +rw-addrmode-cycles+))
       (assoc-value addrmode +addrmode-cycles+))))
