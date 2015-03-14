; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun labels-to-exprs (x)
  (when x
    (? (atom x)
       (? (symbol? x)
          (alet (downcase (symbol-name x))
            (? (| (get-label-0 *labels* !)
                  (get-label-0 *current-scope* !))
               `(get-label ,!)
               x))
          x)
       (. (labels-to-exprs x.)
          (labels-to-exprs .x)))))
