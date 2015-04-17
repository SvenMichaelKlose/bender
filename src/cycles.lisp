(defun instruction-cycles (mnemonic addrmode)
  (| (assoc-value mnemonic +instruction-cycles+)
     (& (member mnemonic +rw-instructions+)
        (assoc-value addrmode +rw-addrmode-cycles+))
     (assoc-value addrmode +addrmode-cycles+)))