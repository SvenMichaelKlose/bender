; bender - Copyright (c) 2015 Sven Michael Klose <pixel@hugbox.org>

(defvar a nil)  ; accumulator
(defvar x nil)  ; index X
(defvar y nil)  ; index Y
(defvar n nil)  ; N flag
(defvar z nil)  ; Z flag
(defvar c nil)  ; C flag
(defvar v nil)  ; V flag
(defvar d nil)  ; D flag
(defvar r nil)  ; operand
(defvar m nil)  ; memory

(defun e-arith-flags ()
  (= n (bit-and a #x80))
  (= z (zero? a)))

(defun e-adc ()
  (alet (+ a r c)
    (= v (bit-and (~ (bit-and (bit-xor a r)) (bit-xor a !)) #x80))
    (= c (< #xff !))
    (= a !))
  (e-arith-flags))

(defun e-sbc ()
  (let neg (++ (bit-xor r #xff))
    (alet (+ a neg c)
      (= v (bit-and (~ (bit-and (bit-xor a neg)) (bit-xor a !)) #x80))
      (= c (< #xff !))
      (= a !)))
  (e-arith-flags))

(defun e-and ()
  (= a (bit-and a r))
  (e-arith-flags))

(defun e-ora ()
  (= a (bit-or a r))
  (e-arith-flags))

(defun e-asl ()
  (= c (bit-and r #x80))
  (= a (<< a 1))
  (e-arith-flags))

(defun e-bit ()
  (= v (bit-and r #x40))
  (e-arith-flags))

(defun e-cmp-shared (a)
  (alet (- a r)
    (= n (bit-and r #x80))
    (= z (zero? r))
    (= c (<= 0 r))))

(defun e-cmp () (e-cmp-shared a))
(defun e-cpx () (e-cmp-shared x))
(defun e-cpy () (e-cmp-shared y))

(defun e-dec ()
  (--! r)
  (e-arith-flags))

(defun e-eor ()
  (= a (bit-xor a r))
  (e-arith-flags))

(defun e-inc ()
  (++! r)
  (e-arith-flags))

(defun e-lda ()
  (= a r)
  (e-arith-flags))

(defun e-ldx ()
  (= x r)
  (e-arith-flags))

(defun e-ldy ()
  (= y r)
  (e-arith-flags))

(defun e-lsr ()
  (= c (bit-and r #x01))
  (= a (>> a 1))
  (e-arith-flags))

(defun e-ora ()
  (= a (bit-or a r))
  (e-arith-flags))


(defun e-rol ()
  (alet c
    (= c (bit-and r #x80))
    (= r (bit-or (<< r 1) (? ! 1 0))))
  (e-arith-flags))

(defun e-ror ()
  (alet c
    (= c (bit-and r #x01))
    (= r (bit-or (>> r 1) (? ! #x80 0))))
  (e-arith-flags))

(defun pha ()
  (= (aref m (+ #x0100 s)) a)
  (--! s))

(defun php ()
  (= (aref m (+ #x0100 s)) (e-get-flags))
  (--! s))

(defun pla ()
  (++! s)
  (= a (aref m (+ #x0100 s))))

(defun plp ()
  (++! s)
  (e-set-flags (aref m (+ #x0100 s))))

(defun tax () (= x a))
(defun txa () (= a x))
(defun tay () (= y a))
(defun tya () (= a y))
(defun txs () (= s x))
(defun tsx () (= x s))

(defun e-push-pc ()
  (e-push (high *pc*))
  (e-push (low *pc*)))

(defun e-pop-pc ()
  (= *pc* (+ (* #x0100 (aref m (++ s))) (aref m s)))
  (+! s 2))

(defun brk ()
  (++! *pc*)
  (e-push-pc)
  (= i 1)
  (e-php)
  (= *pc* (+ (* #x0100 (aref m #xffff)) (aref m #xfffe))))

(defun e-jsr ()
  (alet (e-get-word)
    (e-push-pc)
    (= *pc* !)))

(defun e-nop ())

(defun e-rts ()
  (e-pop-pc))

(defun e-rti ()
  (e-plp)
  (e-pop-pc))
