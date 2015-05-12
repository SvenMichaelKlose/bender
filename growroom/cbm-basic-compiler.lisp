; bender – Copyright (c) 2015 Sven Michael Klose <pixel@hugbox.org>

; Let's start with byte integers and strings and go on from there.

(defun number-to-accu (x)
  (?
    (number? x.)    (line out "lda #~A" x.)
    (variable? x.)  (line out "lda ~A" x.)
    (error "Number expected.")))

(defun accu-to-var (x)
  (line out "sta ~A" x))

(defun var-to-ptr (x)
  (line out "lda ~A")
  (line out "sta ptr")
  (line out "lda @(++ ~A)")
  (line out "sta @(++ ptr)"))

(defun load-ptr (x)
  (line out "ldy #0")
  (line out "lda (ptr),y"))

(defun store-ptr (x)
  (line out "ldy #0")
  (line out "sta (ptr),y"))

(defun poke (out x)
  (line out "lda #~A" .x.)
  (line out "sta ~A" .x.))

(defun peek (out x)
  (? (number? x.)      (line out "lda ~A" x.)
     (number-var? x.)  (progn
                         (var-to-ptr out x.)
                         (load-ptr out))
     (error "Number expected.")))

(defun sys (out x)
  (line out "jsr ~A" x.))

(defun for (out x)
  (| (variable? x.)
     (error "Iterator must be a variable."))
  ; Check =
  (| (number-type? ..x.)
     (error "Can't iterate over strings."))
  ; Check TO
  (| (number-type? ....x.)
     (error "Can't iterate over strings."))
  ; Check STEP
  ; Check STEP's value
  (with (from   x.
         to     ..x.
         step   (| ....x. 1))
    (with-for-labels
      (number-to-accu out x.)
      (accu-to-var out iterator)
      (format out "~A:~%" label-restart)
      (var-to-accu out iterator)
      (? (& (number? from)
            (number? to)
            (< to from))
         (op-<= out to label-end)
         (op->= out to label-end))))

(defun next (out x)
  (| (variable? x.)
     (error "Iterator must be a variable."))
  (with-next-labels
    (var-to-accu out iterator)
    (add step)
    (line out "jmp ~A" label-restart)))

(defun cbm-basic-to-asm ()
  (alet (compile-lines)
    (put-file outname (+ (compiled-number-variables)
                         (compiled-array-variables)
                         (compiled-string-variables)
                         (compiled-data)
                         (fetch-file "vic-20/basic-loader.asm")
                         (fetch-file "growroom/cbm-basic-init.asm")
                         !))))
