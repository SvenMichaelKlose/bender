; bender – Copyright (c) 2014–2015 Sven Michael Klose <pixel@hugbox.org>

(defconstant +short+  #x2c)  ;#x30)
(defconstant +medium+ #x3f)  ;#x42)
(defconstant +long+   #x54)  ;#x56)

(defconstant +cbmtype-relocatable+      1)
(defconstant +cbmtype-seq-data-block+   2)
(defconstant +cbmtype-non-relocatable+  3)
(defconstant +cbmtype-seq-file-header+  4)
(defconstant +cbmtype-end-of-tape+      5)

(defun write-pulse (o len)
  (princ (code-char len) o))

(defun write-short (o)
  (write-pulse o +short+))

(defun write-medium (o)
  (write-pulse o +medium+))

(defun write-long (o)
  (write-pulse o +long+))

(defun write-bit (o x)
  (? (zero? x)
     (progn
       (write-short o)
       (write-medium o))
     (progn
       (write-medium o)
       (write-short o))))

(defun write-sod (o)
  (write-long o)
  (write-medium o))

(defun write-eod (o)
  (write-long o)
  (write-short o))

(defvar *chk* 0)

(defun write-byte (o x)
  (= *chk* (bit-xor *chk* x))
  (write-sod o)
  (let chk 1
    (dotimes (i 8)
      (= chk (bit-xor chk (bit-and x 1)))
      (write-bit o (bit-and x 1))
      (= x (>> x 1)))
    (write-bit o chk)))

(defun write-leader (o)
  (format t "Making leader...~%")
  (adotimes #x6400
    (write-short o)))

(defun write-trailer (o)
  (format t "Making trailer...~%")
  (adotimes #x4e
    (write-short o)))

(defun write-interrecord-gap (o)
  (format t "Making interrecord gap...~%")
  (adotimes #x1500
    (write-short o)))

(defun write-interblock-gap (o)
  (format t "Making interblock gap...~%")
  (adotimes #x4f
    (write-short o)))

(defun write-sync (o repeated?)
  (format t "Making ~Async...~%" (? repeated?
                                     "repeated "
                                     ""))
  (dolist (i '(9 8 7 6 5 4 3 2 1))
    (write-byte o (+ (? repeated?
                        0
                        128)
                     i))))

(defun write-header (o type start end name repeated?)
  (when repeated?
    (write-interblock-gap o))
  (write-sync o repeated?)
  (format t "Making ~Aheader...~%" (? repeated?
                                       "repeated "
                                       ""))
  (write-byte o type)
  (= *chk* 0)
  (unless (== 2 type)
    (write-byte o (mod start 256))
    (write-byte o (>> start 8))
    (write-byte o (mod (++ end) 256))
    (write-byte o (>> (++ end) 8))
    (dolist (i (string-list name))
      (write-byte o i))
    (dotimes (i (- 187 (length name)))
      (write-byte o 32))
    (write-byte o *chk*)
    (write-eod o)
    (when repeated?
      (write-trailer o))))

(defun write-data (o data repeated?)
  (? repeated?
     (write-interblock-gap o)
     (write-interrecord-gap o))
  (write-sync o repeated?)
  (format t "Making ~A bytes of ~Adata...~%" (length data)
                                              (? repeated?
                                                 "repeated "
                                                 ""))
  (= *chk* 0)
  (adolist data
    (write-byte o !))
  (write-byte o 0)
  (write-byte o *chk*)
  (write-eod o)
  (when repeated?
    (write-trailer o)))

(defun bin2cbmtap (data name &key (type +cbmtype-relocatable+) start)
  (with (data    (filter [? (< _ 0)
                            (+ 256 _)
                            _]
                         data)
         len     (length data)
         end     (-- (+ start len)))
    (format t "CBM file type is ~A.~%" type)
    (format t "Start: $~A, end: $~A~%"
            (print-hexword start nil)
            (print-hexword end nil))
    (let p (make-string-stream)
      (write-leader p)
      (write-header p type start end name nil)
      (write-header p type start end name t)
      (write-data p data nil)
      (write-data p data t)
      (list-string (queue-list (stream-user-detail p))))))
