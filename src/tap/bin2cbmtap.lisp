; bender – Copyright (c) 2014–2016,2024 Sven Michael Klose <pixel@hugbox.org>

(defconstant +short+  #x2c)  ;#x30)
(defconstant +medium+ #x3f)  ;#x42)
(defconstant +long+   #x54)  ;#x56)

(defconstant +cbmtype-relocatable+      1)
(defconstant +cbmtype-seq-data-block+   2)
(defconstant +cbmtype-non-relocatable+  3)
(defconstant +cbmtype-seq-file-header+  4)
(defconstant +cbmtype-end-of-tape+      5)

(fn write-gap (o)
  (format t "Making empty gap...~%")
  (write-byte 0 o)
  (write-byte #x7a o)
  (write-byte #xfb o)
  (write-byte #x05 o))

(fn write-short (o)
  (write-byte +short+ o))

(fn write-medium (o)
  (write-byte +medium+ o))

(fn write-long (o)
  (write-byte +long+ o))

(fn write-bit (o x)
  (? (== 0 x)
     (progn
       (write-short o)
       (write-medium o))
     (progn
       (write-medium o)
       (write-short o))))

(fn write-sod (o)
  (write-long o)
  (write-medium o))

(fn write-eod (o)
  (write-long o)
  (write-short o))

(var *chk* 0)

(fn write-byte-without-checksumming (o x)
  (? (character? x)         ; TODO: Remove workaround.
     (= x (char-code x)))
  (write-sod o)
  (let chk 1
    (dotimes (i 8)
      (= chk (bit-xor chk (bit-and x 1)))
      (write-bit o (bit-and x 1))
      (= x (>> x 1)))
    (write-bit o chk)))

(fn write-byte-with-checksumming (o x)
  (= *chk* (bit-xor *chk* x))
  (write-byte-without-checksumming o x))

(fn write-leader (o short?)
  (format t "Making leader...~%")
  (adotimes ((? short? #x2000 #x6a00))
    (write-short o)))

(fn write-sync (o repeated?)
  (format t "Making ~Async...~%" (? repeated? "repeated " ""))
  (@ (i '(9 8 7 6 5 4 3 2 1))
    (write-byte-with-checksumming o (+ (? repeated? 0 128) i))))

(fn write-interblock-gap (o)
  (format t "Making interblock gap...~%")
  (adotimes #x4f
    (write-short o)))

(fn write-interrecord-gap (o)
  (format t "Making interrecord gap...~%")
  (adotimes #x1500
    (write-short o)))

(fn write-trailer (o)
  (format t "Making trailer...~%")
  (adotimes #x4e
    (write-short o)))

(fn write-header (o type start end name repeated? short-data?)
  (write-sync o repeated?)
  (format t "Making ~Aheader...~%" (? repeated?  "repeated " ""))
  (= *chk* 0)
  (write-byte-with-checksumming o type)
  (write-byte-with-checksumming o (mod start 256))
  (write-byte-with-checksumming o (>> start 8))
  (write-byte-with-checksumming o (mod end 256))
  (write-byte-with-checksumming o (>> end 8))
  (? (& repeated? short-data?)
     (adotimes 2
       (write-byte-with-checksumming o 0))
     (progn
       (@ (i (string-list name))
         (write-byte-with-checksumming o i))
       (dotimes (i (- 187 (length name)))
         (write-byte-with-checksumming o 32))))
  (write-byte-without-checksumming o *chk*)
  (write-eod o))

(fn write-data (o data repeated? short-data?)
  (write-sync o repeated?)
  (format t "Making ~Adata (~A bytes)...~%"
            (? repeated?  "repeated " "")
            (length data))
  (= *chk* 0)
  (? (& repeated? short-data?)
     (adotimes 2
       (write-byte-with-checksumming o 0))
     (adolist data
       (write-byte-with-checksumming o !)))
  (write-byte-without-checksumming o *chk*)
  (write-eod o))

(fn bin2cbmtap (data name &key (type +cbmtype-relocatable+)
                               start (short-leader? nil) (short-data? nil)
                               (no-gaps? nil))
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
      (| no-gaps?
         (write-gap p))
      (write-leader p short-leader?)
      (write-header p type start end name nil nil)
      (write-interblock-gap p)
      (write-header p type start end name t short-data?)
      (write-trailer p)
      (| no-gaps?
         (write-gap p))
      (write-interrecord-gap p)
      (write-data p data nil nil)
      (write-interblock-gap p)
      (write-data p data t short-data?)
      (write-trailer p)
      (list-string (queue-list (stream-user-detail p))))))
