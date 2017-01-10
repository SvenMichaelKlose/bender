; bender – Copyright (c) 2014–2016 Sven Michael Klose <pixel@hugbox.org>

(defconstant +short+  #x2c)  ;#x30)
(defconstant +medium+ #x3f)  ;#x42)
(defconstant +long+   #x54)  ;#x56)

(defconstant +cbmtype-relocatable+      1)
(defconstant +cbmtype-seq-data-block+   2)
(defconstant +cbmtype-non-relocatable+  3)
(defconstant +cbmtype-seq-file-header+  4)
(defconstant +cbmtype-end-of-tape+      5)

(defun write-gap (o)
  (format t "Making empty gap...~%")
  (write-byte 0 o)
  (write-byte #x7a o)
  (write-byte #xfb o)
  (write-byte #x05 o))

(defun write-short (o)
  (write-byte +short+ o))

(defun write-medium (o)
  (write-byte +medium+ o))

(defun write-long (o)
  (write-byte +long+ o))

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

(defun write-byte-without-checksumming (o x)
  (? (character? x)         ; TODO: Remove workaround.
     (= x (char-code x)))
  (write-sod o)
  (let chk 1
    (dotimes (i 8)
      (= chk (bit-xor chk (bit-and x 1)))
      (write-bit o (bit-and x 1))
      (= x (>> x 1)))
    (write-bit o chk)))

(defun write-byte-with-checksumming (o x)
  (= *chk* (bit-xor *chk* x))
  (write-byte-without-checksumming o x))

(defun write-leader (o)
  (format t "Making leader...~%")
  (adotimes #x6a00
    (write-short o)))

(defun write-sync (o repeated?)
  (format t "Making ~Async...~%" (? repeated? "repeated " ""))
  (dolist (i '(9 8 7 6 5 4 3 2 1))
    (write-byte-with-checksumming o (+ (? repeated? 0 128) i))))

(defun write-interblock-gap (o)
  (format t "Making interblock gap...~%")
  (adotimes #x4f
    (write-short o)))

(defun write-interrecord-gap (o)
  (format t "Making interrecord gap...~%")
  (adotimes #x1500
    (write-short o)))

(defun write-trailer (o)
  (format t "Making trailer...~%")
  (adotimes #x4e
    (write-short o)))

(defun write-header (o type start end name repeated?)
  (write-sync o repeated?)
  (format t "Making ~Aheader...~%" (? repeated?  "repeated " ""))
  (= *chk* 0)
  (write-byte-with-checksumming o type)
  (write-byte-with-checksumming o (mod start 256))
  (write-byte-with-checksumming o (>> start 8))
  (write-byte-with-checksumming o (mod end 256))
  (write-byte-with-checksumming o (>> end 8))
  (dolist (i (string-list name))
    (write-byte-with-checksumming o i))
  (dotimes (i (- 187 (length name)))
    (write-byte-with-checksumming o 32))
  (write-byte-without-checksumming o *chk*)
  (write-eod o))

(defun write-data (o data repeated?)
  (write-sync o repeated?)
  (format t "Making ~Adata (~A bytes)...~%"
          (? repeated?  "repeated " "")
          (length data))
  (= *chk* 0)
  (adolist data
    (write-byte-with-checksumming o !))
  (write-byte-without-checksumming o *chk*)
  (write-eod o))

(defun bin2cbmtap (data name &key (type +cbmtype-relocatable+) start)
  (with (data  (filter [? (< _ 0)
                          (+ 256 _)
                          _]
                       (@ #'char-code data))
         len   (length data)
         end   (+ start len))
    (format t "CBM file name is '~A'.~%" (subseq name 0 16))
    (format t "CBM file type is ~A.~%" type)
    (format t "Start: $~A, end: $~A~%"
            (print-hexword start nil)
            (print-hexword end nil))
    (let p (make-string-stream)
      (write-gap p)
      (write-leader p)
      (write-header p type start end name nil)
      (write-interblock-gap p)
      (write-header p type start end name :repeated)
      (write-trailer p)
      (write-gap p)
      (write-interrecord-gap p)
      (write-data p data nil)
      (write-interblock-gap p)
      (write-data p data :repeated)
      (write-trailer p)
      (list-string (queue-list (stream-user-detail p))))))
