;;; :FILE-CREATED <Timestamp: #{2011-08-30T20:40:55-04:00Z}#{11352} - by MON>
;;; :FILE tiff4cl-FORK/tiff4cl-conditions.lisp
;;; ==============================

(in-package #:tiff4cl)

(define-condition tiff-error (error) ())

;; :SEE `parse-endianness'
(define-condition not-a-tiff (tiff-error) ())

;; :SEE `parse-TIFF-stream'
(define-condition wrong-version (tiff-error)
  ((version :initarg :version
	    :reader tiff-error-version)))

;; :SEE `read-IFD-pointer'
(define-condition invalid-pointer (tiff-error)
  ((pointer :initarg :pointer
	    :reader tiff-error-pointer)))


;;; ==============================
;;; EOF
