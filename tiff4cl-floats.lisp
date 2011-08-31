;;; :FILE-CREATED <Timestamp: #{2011-08-30T21:18:02-04:00Z}#{11352} - by MON>
;;; :FILE tiff4cl-FORK/tiff4cl-floats.lisp
;;; ==============================

;;; ==============================
;;; code.lisp --- IEEE floating point encoding/decoding

;;; Copyright (C) 2009 by Walter C. Pelissero

;;; Author: Walter C. Pelissero <walter@pelissero.de>
;;; Project: ie3fp

#+cmu (ext:file-comment "$Module: code.lisp $")

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;; 02111-1307 USA

;;; Code inspired by work done by Marijn Haverbeke.
;;; The original code can be found here:
;;; http://common-lisp.net/project/ieee-floats/

;; (defpackage :ie3fp
;;   (:nicknames :ieee-754)
;;   (:use :common-lisp)
;;   (:export #:make-encoder
;; 	   #:make-decoder
;; 	   #:encode-IEEE-float
;; 	   #:decode-IEEE-float
;; 	   #:encode-IEEE-double
;; 	   #:decode-IEEE-double
;; 	   #:encode-IEEE-quad
;; 	   #:decode-IEEE-quad))

;; (in-package :ie3fp)

(in-package #:tiff4cl)

(defmacro make-encoder (name exponent-length significand-length)
  (let ((total-length (+ 1 exponent-length significand-length)))
    `(defun ,name (float)
       (declare (float float)
		(optimize (speed 3) (debug 0) (safety 0)))
       (multiple-value-bind (significand exponent sign) (integer-decode-float float)
	 (declare (type integer significand)
		  (type fixnum exponent sign))
	 (let* ((len (integer-length significand))
		(delta (- ,significand-length len)))
	   (setf exponent (+ exponent (1- len) ,(1- (expt 2 (1- exponent-length)))))
	   (unless (< exponent ,(expt 2 exponent-length))
	     (error "Floating point number ~A too big; can't be encoded with ~A bit exponent."
		    float ,exponent-length))
	   (if (< exponent 0)
	       (setf significand (ash (logand (ash significand delta)
					      ,(1- (expt 2 significand-length)))
				      exponent)
		     exponent 0)
	       (setf significand (logand (ash significand (1+ delta))
					 ,(1- (expt 2 significand-length)))))
	   (let ((encoded 0))
	     (declare (type (unsigned-byte ,total-length) encoded))
	     (setf (ldb (byte 1 ,(1- total-length)) encoded) (if (= sign 1) 0 1)
		   (ldb (byte ,exponent-length ,significand-length) encoded) exponent
		   (ldb (byte ,significand-length 0) encoded) significand)
	     encoded))))))

(defmacro make-decoder (name exponent-length significand-length)
  (let ((total-length (+ 1 exponent-length significand-length)))
    `(defun ,name (encoded)
       (declare (type (unsigned-byte ,total-length) encoded)
		(optimize (speed 3) (debug 0) (safety 0)))
       (let ((negative (ldb (byte 1 ,(1- total-length)) encoded))
	     (exponent (ldb (byte ,exponent-length ,significand-length) encoded))
	     (significand (ldb (byte ,significand-length 0) encoded)))
	 (if (zerop exponent)
	     (setf exponent 1)
	     ;; add the "hidden bit"
	     (setf (ldb (byte 1 ,significand-length) significand) 1))
	 (scale-float (float (if (zerop negative)
				 significand
				 (- significand))
			     ,(if (> total-length 32)
				  1.0d0
				  1.0))
		      (- exponent ,(+ (1- (expt 2 (1- exponent-length)))
				      significand-length)))))))


(make-encoder encode-ieee-float 8 23)
(make-decoder decode-ieee-float 8 23)

(make-encoder encode-ieee-double 11 52)
(make-decoder decode-ieee-double 11 52)

(make-encoder encode-IEEE-quad 15 112)
(make-decoder decode-IEEE-quad 15 112)

;;; ==============================
;;; EOF
