;;; :FILE-CREATED <Timestamp: #{2011-08-31T16:54:04-04:00Z}#{11353} - by MON>
;;; tiff4cl-FORK/nikon/tiff4cl-nikon.lisp
;;; ==============================


(in-package #:tiff4cl)

;; flashFirmware -- flash firmware decoding
(defparameter *flash-firmware
  '(( #(0 0) "n/a")
    ( #(1 1) "1.01 (SB-800 or Metz 58 AF-1)")
    ( #(1 3) "1.03 (SB-800)")
    ( #(2 1) "2.01 (SB-800)")
    ( #(2 4) "2.04 (SB-600)")
    ( #(2 5) "2.05 (SB-600)")
    ( #(3 1) "3.01 (SU-800 Remote Commander)")
    ( #(4 1) "4.01 (SB-400)")
    ( #(4 2) "4.02 (SB-400)")
    ( #(4 4) "4.04 (SB-400)")
    ( #(5 1) "5.01 (SB-900)")
    ( #(5 2) "5.02 (SB-900)")))


;; flashGNDistance -- flash Guide Number (GN) distance settings


(defparameter *flash-guide-number-distance*
  '((0  . "0.0 m")
    (1  . "0.1 m")
    (2  . "0.2 m")
    (3  . "0.3 m")
    (4  . "0.4 m")
    (5  . "0.5 m")
    (6  . "0.6 m")
    (7  . "0.7 m")
    (8  . "0.8 m")
    (9  . "0.9 m")
    (10 . "1.0 m")
    (11 . "1.1 m")
    (12 . "1.3 m")
    (13 . "1.4 m")
    (14 . "1.6 m")
    (15 . "1.8 m")
    (16 . "2.0 m")
    (17 . "2.2 m")
    (18 . "2.5 m")
    (19 . "2.8 m")
    (20 . "3.2 m")
    (21 . "3.6 m")
    (22 . "4.0 m")
    (23 . "4.5 m")
    (24 . "5.0 m")
    (25 . "5.6 m")
    (26 . "6.3 m")
    (27 . "7.1 m")
    (28 . "8.0 m")
    (29 . "9.0 m")
    (30 . "10.0 m")
    (31 . "11.0 m")
    (32 . "13.0 m")
    (33 . "14.0 m")
    (34 . "16.0 m")
    (35 . "18.0 m")
    (36 . "20.0 m")
    (255 . "n/a")))



;; flashControlMode -- flash control mode values
(defparameter *flash-control-mode*
  '((0 . "Off")
    (1 . "iTTL-BL")
    (2 . "iTTL")
    (3 . "Auto Aperture")
    (4 . "Automatic")
    (5 . "GN (distance priority)")
    (6 . "Manual")
    (7 . "Repeating Flash")))


;; retouchValues
(defparameter *retouch-values*
  '(( 0 . "None")
    ( 3 . "B & W")
    ( 4 . "Sepia")
    ( 5 . "Trim")
    ( 6 . "Small Picture")
    ( 7 . "D-Lighting")
    ( 8 . "Red Eye")
    ( 9 . "Cyanotype")
    (10 . "Sky Light")
    (11 . "Warm Tone")
    (12 . "Color Custom")
    (13 . "Image Overlay")
    (14 . "Red Intensifier")
    (15 . "Green Intensifier")
    (16 . "Blue Intensifier")
    (17 . "Cross Screen")
    (18 . "Quick Retouch")
    (19 . "NEF Processing")
    (23 . "Distortion Control")
    (25 . "Fisheye")
    (26 . "Straighten")
    (29 . "Perspective Control")
    (30 . "Color Outline")
    (31 . "Soft Filter")
    (33 . "Miniature Effect")))

;; maker-note
;; "maker-note-version"
;; #x0001 "0210" or (map 'string #'code-char  #(#x00 #x01 #x00 #x00)) e.g. "\x00\x01\x00\x00" 
;; :length 4 
;; :type   7
(1 
 (
;; ISO
;; #x0002
;; :type 3
;; :length 2


;;; ==============================
;;; EOF
