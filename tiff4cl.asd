;;; :FILE-CREATED <Timestamp: #{2011-08-30T21:22:21-04:00Z}#{11352} - by MON>
;;; :FILE tiff4cl-FORK/tiff4cl.asd
;;; ==============================

(in-package :cl-user)

(defpackage #:tiff4cl-system (:use :common-lisp :asdf))

(in-package #:tiff4cl-system)

(defsystem :tiff4cl
  :name "TIFF4CL"
  :author "Walter C. Pelissero <walter@pelissero.de>"
  :maintainer "Walter C. Pelissero <walter@pelissero.de>"
  ;; :version "0.0"
  :description "TIFF access primitives"
  :long-description "A TIFF file parser that reads the tags leaving the actual image data alone."
  :licence "LGPL"
  ;; :depends-on (:ie3fp) ;; `ieee-754:decode-ieee-float'
  :serial t
  :components
  ((:file "package")
   (:file "tiff4cl-util")
   (:file "tiff4cl-specials")
   (:file "tiff4cl-conditions")
   (:file "tiff4cl-floats")
   (:file "tiff4cl-tiff")))

;;; ==============================
;;; EOF
