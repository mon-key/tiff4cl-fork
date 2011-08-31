;;; :FILE-CREATED <Timestamp: #{2011-08-30T21:23:57-04:00Z}#{11352} - by MON>
;;; :FILE tiff4cl-FORK/tiff4cl-util.lisp
;;; ==============================

(in-package #:tiff4cl)

;; :SEE info node (info "(sbcl)Defining Constants")
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro make-read-sequence (name buffer)
  `(defun ,name (stream length &key (eof-errorp t) eof-value)
     (let* ((buffer ,buffer)
	    (n (read-sequence buffer stream)))
       (cond ((= n length)
	      buffer)
	     (eof-errorp
	      (error "EOF."))
	     (t eof-value)))))

(make-read-sequence read-bytes (make-sequence '(vector (unsigned-byte 8)) length))
;; (make-read-sequence read-string (make-string length))

(defun decode-integer-be (sequence &key (start 0) end)
  "Decode a big-endian sequence of bytes as an integer and return it."
  (loop
     with value = 0
     for i from start below (or end (length sequence))
     do (setf value (logior (ash value 8)
			    (elt sequence i)))
     finally (return value)))

(defun decode-integer-le (sequence &key (start 0) end)
  "Decode a big-endian sequence of bytes as an integer and return it."
  (loop
     with value = 0
     for i from (1- (or end (length sequence))) downto start
     do (setf value (logior (ash value 8)
			    (elt sequence i)))
     finally (return value)))

(defun read-16bit-be (stream)
  (decode-integer-be (read-bytes stream 2)))

(defun read-16bit-le (stream)
  (decode-integer-le (read-bytes stream 2)))

(defun read-32bit-be (stream)
  (decode-integer-be (read-bytes stream 4)))

(defun read-32bit-le (stream)
  (decode-integer-le (read-bytes stream 4)))


;;; ==============================
;;; EOF
