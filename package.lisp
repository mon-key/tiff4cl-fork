;;; :FILE-CREATED <Timestamp: #{2011-08-30T21:18:53-04:00Z}#{11352} - by MON>
;;; :FILE tiff4cl-FORK/package.lisp
;;; ==============================

(in-package :cl-user)

(defpackage #:tiff4cl (:use #:common-lisp)
            ;; (:nicknames :tiff)
            (:export #:parse-tiff
                     #:print-tiff-tags
                     #:map-tiff-tags
                     #:tiff-extract-tags
                     ;; accessors
                     #:ifd-tags
                     #:tag-id
                     #:tag-type
                     #:tag-value))

;;; ==============================
;;; EOF
