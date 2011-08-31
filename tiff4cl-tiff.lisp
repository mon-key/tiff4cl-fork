;;; tiff4cl.lisp --- TIFF parsing

;;; Copyright (C) 2009 by Walter C. Pelissero

;;; Author: Walter C. Pelissero <walter@pelissero.de>
;;; Project: tiff4cl

#+cmu (ext:file-comment "$Module: tiff.lisp $")

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

(in-package #:tiff4cl)

(defclass tiff-stream ()
  ((stream :initarg :stream
	   :reader tiff-stream)
   (start :initarg :start
	  :initform 0
	  :type integer
	  :reader tiff-start)
   (end :initarg :end
	:initform nil
	:type (or integer null)
	:reader tiff-end)))

(defclass tiff-little-endian-stream (tiff-stream) ())

(defclass tiff-big-endian-stream (tiff-stream) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric decode-integer (tiff buffer &key start end))

(defmethod decode-integer ((tiff tiff-little-endian-stream) buffer &key (start 0) end)
  (decode-integer-le buffer
		     :start start
		     :end end))
(defmethod decode-integer ((tiff tiff-big-endian-stream) buffer &key (start 0) end)
  (decode-integer-be buffer
		     :start start
		     :end end))

(defgeneric read-16bit (tiff))

(defmethod read-16bit ((tiff tiff-little-endian-stream))
  (read-16bit-le (tiff-stream tiff)))

(defmethod read-16bit ((tiff tiff-big-endian-stream))
  (read-16bit-be (tiff-stream tiff)))

(defgeneric read-32bit (tiff))

(defmethod read-32bit ((tiff tiff-little-endian-stream))
  (read-32bit-le (tiff-stream tiff)))

(defmethod read-32bit ((tiff tiff-big-endian-stream))
  (read-32bit-be (tiff-stream tiff)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-endianness (stream)
  (let ((buffer (read-bytes stream 2)))
    (cond ((equalp +little-endian-signature+ buffer)
	   ;; Intel's little endian
	   :little-endian)
	  ((equalp +big-endian-signature+ buffer)
	   ;; Motorola's big endian
	   :big-endian)
	  (t
	   (error 'not-a-tiff)))))

(defun parse-file-format-version (tiff)
  (read-16bit tiff))

(defun read-ifd-pointer (tiff)
  (let ((pointer (read-32bit tiff)))
    (when (or (and (< pointer 8)
		   ;; the last pointer is always zero
		   (not (zerop pointer)))
	      (not (zerop (mod pointer 2)))
	      (let ((end (tiff-end tiff)))
		(when end
		  (> pointer (- end (tiff-start tiff))))))
      (error 'invalid-pointer :pointer pointer))
    pointer))

(defun tiff-position (tiff &optional position)
  (- (tiff-start tiff)
     (if position
	 (progn
	   (assert
	    (file-position (tiff-stream tiff)
			   (+ position (tiff-start tiff))))
	   position)
	 (file-position (tiff-stream tiff)))))

(defun read-ifd-tags-number (tiff)
  (read-16bit tiff))

(defun tag-type-size (type)
  (caddr (find type +tag-types+ :key #'cadr)))

(defun fetch-tag-data (file address length)
  (tiff-position file (decode-integer file address))
  (read-bytes (tiff-stream file) length))

(defclass tiff-tag ()
  ((id :initarg :id
       :reader tag-id)
   (type :initarg :type
	 :reader tag-type)
   (value :initarg :value
	  :reader tag-value)))

(defclass tiff-ifd ()
  ((tags :initarg :tags
	 :type list
	 :reader ifd-tags)
   (next :initarg :next
	 :reader ifd-next)))

(defmethod print-object ((object tiff-tag) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (object stream :type t)
	(format stream "~A" (tag-id object)))))

(defun get-tag-value (file type count data)
  (let ((item-size (tag-type-size type)))
    (if item-size
	(let ((max-positive (ash 1 (1- (* 8 item-size)))))
	  (labels ((maybe-negative (x)
		     (if (< x max-positive)
			 x
			 (lognot x)))
		   (parse-byte (buffer pos)
		     (aref buffer pos))
		   (parse-int (buffer start end)
		     (decode-integer file buffer :start start :end end))
		   (parse-data-item (buffer pos)
		     (case type
		       (:byte  
                        (parse-byte buffer pos))
		       (:ascii  
                        (code-char (aref buffer pos)))
		       ((:short :long)
			(parse-int buffer (* pos item-size) (* (1+ pos) item-size)))
		       (:rational
			(/ (parse-int buffer
					  (* pos item-size)
					  (+ (* pos item-size) (/ item-size 2)))
			   (parse-int buffer
					  (+ (* pos item-size) (/ item-size 2))
					  (* (1+ pos) item-size))))
		       (:signed-byte
			(maybe-negative (parse-byte buffer pos)))
		       (:undefined
			(aref buffer pos))
		       ((:signed-short :signed-long)
			(maybe-negative
			 (parse-int buffer
					(* pos item-size)
					(* (1+ pos) item-size))))
		       (:signed-rational
			(/ (maybe-negative
			    (parse-int buffer
					   (* pos item-size)
					   (+ (* pos item-size) (/ item-size 2))))
			   (maybe-negative
			    (parse-int buffer
					   (+ (* pos item-size) (/ item-size 2))
					   (* (1+ pos) item-size)))))
		       (:float
			(decode-ieee-float ;; ieee-754:decode-ieee-float
			 (parse-int buffer
					(* pos item-size)
					(* (1+ pos) item-size))))
		       (:double
			(decode-ieee-double ;; ieee-754:decode-ieee-double
			 (parse-int buffer
					(* pos item-size)
					(* (1+ pos) item-size))))))
		   (parse-tag-data (buffer)
		     (if (= 1 count)
			 (parse-data-item buffer 0)
			 (loop
			    with result = (make-array count)
			    for i from 0 below count
			    do (setf (aref result i) (parse-data-item buffer i))
			    finally (return (if (eq type :ascii)
						(coerce (subseq result 0
								;; get rid of the trailing null
								(1- (length result)))
							'string)
						result))))))
	    (cond ((> (* item-size count) 4)
		   (parse-tag-data
		    (fetch-tag-data file data (* item-size count))))
		  (t
		   (parse-tag-data data)))))
	;; the value is in the data field itself
	data)))

(defun parse-flash-value (value)
  (let ((features '()))
    (macrolet ((bf (feature bit)
                 `(when (logbitp ,bit value)
                    (push ,feature features))))
      (bf :fired 0)
      (when (logbitp 2 value)
        (push (if (logbitp 1 value)
                  :strobe-detected
                  :strobe-undetected)
              features))
      (case (ldb (byte 2 3) value)
        (1 (push :flash-forced-on features))
        (2 (push :flash-disabled features))
        (3 (push :auto-mode features)))
      (bf :flash-present 5)
      (bf :red-eye-suppression 6))
    features))

(defun parse-user-comment-value (value)
  (labels ((starts-with (prefix)
             (loop 
                for i across prefix
                for j across value
                always (= i j)))
           (string-all-empty-check (mapped-comment)
             ;; For reading the value of an empty user-comment this should be fine.
             ;; However, if we start writing it back out it may not be...
             (if (every #'(lambda (x) (char= #\SPACE x)) mapped-comment)
                 (make-string 0 :element-type 'character :initial-element #\nul)
                 mapped-comment))
           (map-comment ()
             (let ((first-null (or (position 0 value :start 8)
                                   (length value))))
               (setf first-null
                     (map 'string #'code-char (subseq value 8 first-null)))
               (string-all-empty-check first-null))))
    (if (or (starts-with #(0 0 0 0 0 0 0 0))
            ;; (starts-with #(#x41 #x53 #x43 #x49 #x49))
            (starts-with #(65 83 67 73 73)))
        ;; ASCII encoding
        (map-comment)
        value)))

(defun interpret-tag-value (file id value)
  (case id
    ((:exif-ifd :gps-ifd :interoperability-ifd)
     (parse-ifd file value))
    ((:flashpix-version :exif-version)
     (let ((str (map 'string #'code-char value)))
       (cons (parse-integer str :start 0 :end 2)
	     (parse-integer str :start 2))))
    (:flash
     ;; (let ((features '()))
     ;;   (macrolet ((bf (feature bit)
     ;;    	    `(when (logbitp ,bit value)
     ;;    	       (push ,feature features))))
     ;;     (bf :fired 0)
     ;;     (when (logbitp 2 value)
     ;;       (push (if (logbitp 1 value)
     ;;    	     :strobe-detected
     ;;    	     :strobe-undetected)
     ;;    	 features))
     ;;     (case (ldb (byte 2 3) value)
     ;;       (1 (push :flash-forced-on features))
     ;;       (2 (push :flash-disabled features))
     ;;       (3 (push :auto-mode features)))
     ;;     (bf :flash-present 5)
     ;;     (bf :red-eye-suppression 6))
     ;;   features)
     (parse-flash-value value)
     )
    ;; Following adapted from zpb-exif's exif-type parser for user-comment
    (:user-comment 
     ;; (labels ((starts-with (prefix)
     ;;            (loop 
     ;;               for i across prefix
     ;;               for j across value
     ;;               always (= i j)))
     ;;          (string-all-empty-check (mapped-comment)
     ;;            ;; For reading the value of an empty user-comment this should be fine.
     ;;            ;; However, if we start writing it back out it may not be...
     ;;            (if (every #'(lambda (x) (char= #\SPACE x)) mapped-comment)
     ;;                (make-string 0 :element-type 'character :initial-element #\nul)
     ;;                mapped-comment))
     ;;          (map-comment ()
     ;;            (let ((first-null (or (position 0 value :start 8)
     ;;                                  (length value))))
     ;;              (setf first-null
     ;;                    (map 'string #'code-char (subseq value 8 first-null)))
     ;;              (string-all-empty-check first-null))))
     ;;   (if (or (starts-with #(0 0 0 0 0 0 0 0))
     ;;           ;; (starts-with #(#x41 #x53 #x43 #x49 #x49))
     ;;           (starts-with #(65 83 67 73 73)))
     ;;       ;; ASCII encoding
     ;;       (map-comment)
     ;;       value))
     (parse-user-comment-value value))
    (t
     (let ((symbolic-mapping (assoc value (caddr (find id *tag-ids* :key #'cadr)))))
       (if symbolic-mapping
	   (cadr symbolic-mapping)
	   value)))))

(defun tag-id->symbol (n)
  (cadr (assoc n *tag-ids*)))

(defun read-tag-id (tiff)
  (let ((x (read-16bit tiff)))
    (or (tag-id->symbol x) x)))

(defun tag-type->symbol (n)
  (cadr (assoc n +tag-types+)))

(defun read-tag-type (tiff)
  (let ((x (read-16bit tiff)))
    (or (tag-type->symbol x)
	x)))

(defun read-tag-values-count (tiff)
  (read-32bit tiff))

(defun read-tag-data (tiff)
  (read-bytes (tiff-stream tiff) 4))

(defun parse-tag (tiff)
  (let* ((id (read-tag-id tiff))
	 (type (read-tag-type tiff))
	 (n (read-tag-values-count tiff))
	 (data (read-tag-data tiff)))
    (list id type n data)))

(defun parse-ifd (tiff &optional position)
  (when position
    (tiff-position tiff position))
  ;; First read the tags data and then create the objects, because
  ;; get-tag-value can seek through the file to fetch the data values,
  ;; thus disrupting the sequential read we do here.
  (let* ((tags (loop
		  for i from 0 below (read-ifd-tags-number tiff)
		  collect (parse-tag tiff)))
	 (next-ifd (read-ifd-pointer tiff))
         ;; :NOTE Why shouldn't we use an flet'd function for the lambda form
         ;; below?  Likewise, why not separate the entire body of the
         ;; tag-objects binding to a dedicated function?
	 (tag-objects (mapcar #'(lambda (tag)
				  (destructuring-bind (id type count data) tag
				    (let ((value (get-tag-value tiff type count data)))
				      (make-instance 'tiff-tag
						     :id id
						     :type type
						     :value (interpret-tag-value tiff id value)))))
			      tags)))
    (make-instance 'tiff-ifd :tags tag-objects :next next-ifd)))

(defun parse-tiff-stream (stream &optional end)
  (let* ((start (file-position stream))
	 (endianness (parse-endianness stream))
	 (tiff (make-instance (if (eq endianness :little-endian)
				  'tiff-little-endian-stream
				  'tiff-big-endian-stream)
			      :start start
			      :end end
			      :stream stream))
	 (version (parse-file-format-version tiff)))
    (unless (= version +tiff-version+)
      (error 'wrong-version :version version))
    (let ((ifds '())
	  (address (read-ifd-pointer tiff)))
      (loop
	 (when (zerop address)
	   (return))
	 (let ((ifd (parse-ifd tiff address)))
	   (push ifd ifds)
	   (setf address (ifd-next ifd))))
      ifds)))

(defun parse-tiff (file &key start end)
  (if (streamp file)
      (progn
	(when start
	  (file-position file start))
	(parse-tiff-stream file end))
      (with-open-file (stream file)
	(when start
	  (file-position stream start))
	(parse-tiff-stream stream end))))

(defun map-ifd-tags (function ifd)
  (dolist (tag (ifd-tags ifd))
    (let ((value (tag-value tag)))
      (if (typep value 'tiff-ifd)
	  (map-ifd-tags function value)
	  (funcall function tag)))))

(defun map-tiff-tags (function ifds)
  (dolist (ifd ifds)
    (map-ifd-tags function ifd)))

(defun tiff-extract-tags (ifds tag-ids)
  (let ((result '()))
    (map-tiff-tags #'(lambda (tag)
		       (when (or (eq tag-ids t)
				 (find (tag-id tag) tag-ids))
			 (push (cons (tag-id tag) (tag-value tag))
			       result)))
		   ifds)
    result))

(defun print-tiff-tags (ifds &optional stream)
  (unless stream
    (setf stream *standard-output*))
  (labels ((print-ifd (ifd indent)
	     (loop
		for tag in (ifd-tags ifd)
		for value = (tag-value tag)
		if (typep value 'tiff-ifd)
		do
		  (format stream "~vT~A:~%" indent (tag-id tag))
		  (print-ifd value (+ 2 indent))
		else do
		  (format stream "~vT~A = ~S~%" indent (tag-id tag) value))))
    (loop
       for ifd in ifds
       for i from 0
       do
	 (format stream "~&IFD ~A:~%" i)
	 (print-ifd ifd 2))))

;;; ==============================
;;; EOF
