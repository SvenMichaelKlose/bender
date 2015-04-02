; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@copei.de>

(defun labels-to-exprs (x)
  (with (f [| (? (get-label _ :required? nil)
                 `(get-label ',_)
                 (let n (symbol-name _)
                   (when (< 1 (length n))
                     (alet (make-symbol (subseq n 1))
                       (? (get-label ! :required? nil)
                          (case (elt n 0) :test #'==
                            #\-  `(get-previous-label ',!)
                            #\+  `(get-next-label ',!)
                            #\<  `(low ,(f !))
                            #\>  `(low ,(f !))))))))
              _])
    (when x
      (? (atom x)
         (? (symbol? x)
            (f x)
            x)
         (. (labels-to-exprs x.)
            (labels-to-exprs .x))))))
