;;; :FILE-CREATED <Timestamp: #{2011-08-30T20:43:23-04:00Z}#{11352} - by MON>
;;; :FILE tiff4cl-FORK/tiff4cl-specials.lisp
;;; ==============================

(in-package #:tiff4cl)

;; :WAS (defconstant +little-endian-signature+
(define-constant +little-endian-signature+ 
    (map 'vector #'char-code "II"))

;; :WAS (defconstant +big-endian-signature+
(define-constant +big-endian-signature+
    (map 'vector #'char-code "MM"))

;; (equal (type-of (map 'vector #'char-code "MM"))
;;        (type-of (map 'simple-vector #'char-code "MM")))

(defconstant +tiff-version+ 42
  "What was the question?")

;; :NOTE :SEE `zpb::*type-readers*' zpb-exif/exif.lisp
;; :WAS (defconstant +tag-types+
(define-constant +tag-types+
    '((1  :byte 1)               ; get-byte 0
      (2  :ascii 1)              ; get-ascii 1
      (3  :short 2)              ; get-short 2
      (4  :long 4)               ; get-long 3
      (5  :rational 8)           ; get-rational 5
      (6  :signed-byte 1)        ; get-byte 0 ???
      (7  :undefined 1)
      (8  :signed-short 2)
      (9  :signed-long 4)        ; get-slong 9
      (10 :signed-rational 8)    ; get-srational 10
      (11 :float 4)
      (12 :double 8))
  "List of supported tag types.
Every type is a triplet consisting of: 
a numeric id, a keyword id, and a length in bytes.")

;;; ==============================
;;
;; Basic structure of uncompressed data files
;;
;;     ,-<[          <TIFF-HEADER>                  ]
;;     `->[ 0th IFD for primary image data ]
;;         | <Image-Width> 
;;         |     ...
;;         | <STRIP-OFFSETS>-------------------------,
;;         |     ...                                 .
;;     ,--<| <EXIF-IFD-POINTER>                      .
;;     .   | <GPS-IFD-POINTER>-----------------,     .
;;   ,-+--<| <NEXT-IFD-POINTER>                .     .
;;   . .   [ ... Value of 0th IFD ... ]        .     .
;;   . `->[ <EXIF-IFD> (EXIF Private Tag) ]    .     .
;;   .      | <EXIF-VERSION>                   .     .
;;   .      | <DATE-TIME-DIGITIZED>            .     .
;;   .      |     ...                          .     .
;;   .     [ ... Value of EXIF IFD ... ]       .     .
;;   .    [ <GPS-IFD> (GPS Info Tag) ]<--------'     .
;;   .      | <GPS-VERSION>                          .
;;   .      |     ...                                .
;;   .     [ ... Value of GPS IFD ... ]              .
;;   `--->[ 1st IFD for thumbnail data ]             .
;;         | <IMAGE-WIDTH>                           .
;;         |   ...                                   .
;;         | <STRIP-OFFSETS>------------------,      .
;;         |   ...                            .      .
;;         [ ... Value of 1st IFD ... ]       .      .
;;        [ ... Thumbnail Data ...   ]<-------'      .
;;        [ Primary Image Data ]<--------------------'
;;         | { Strip1    
;;         |   Strip2
;;         |    ...
;;         |   StripN }
;;
;;; ==============================
;;
;; for use with tiff.h 
;; (save-excursion 
;;    (save-restriction 
;;     (narrow-to-region (region-beginning) (region-end))
;;     (goto-char (buffer-end 0))
;;     (while (search-forward-regexp "\\(/\\*\\(.*\\)\\*/\\)$" nil t) (replace-match ";;\\2"))))
;; 
(defparameter *tag-ids*
  '((254 :new-subfile-type)          ; subfile data descriptor 
    ;; ((1 :FILETYPE-REDUCED-IMAGE)  ; reduced resolution version
    ;;  (2 :FILETYPE-PAGE)           ; one page of many
    ;;  (4 :FILETYPE0-MASK))         ; transparency mask
    (255 :subfile-type)              ; kind of data in subfile
    ;; (1 :OFILETYPE-IMAGE)          ; full resolution image data
    ;; (2 :OFILETYPE-REDUCEDIMAGE)   ; reduced size image data
    ;; (3 :OFILETYPE-PAGE)           ; one page of many
    (256 :image-width) ; image width in pixels
    (257 :image-length)    ;; image height in pixels -- :NOTE exiftool reports this as ImageHeight
    (258 :bits-per-sample) ;; bits per channel (sample)
    
    (259 :compression ; data compression technique
     ((1 nil)         ; no compression
      (2 :ccitt-1d)   ; CCITT modified Huffman RLE
      (3 :fax-group3) ; CCITT Group 3 fax encoding / CCITT T.4 (TIFF 6 name)
      (4 :fax-group4) ; CCITT Group 4 fax encoding / CCITT T.6 (TIFF 6 name)
      (5 :lzw)        ; Lempel-Ziv & Welch
      (6 :jpeg)       ; 6.0 JPEG
      (7 :jpeg2)      ; JPEG DCT compression
      (8 :zip)                     ; :adobe-deflate -- Photoshop extension  -- :NOTE exiftool reports this as "Adobe Deflate". 
      (32766 :next)                ; NeXT 2-bit RLE
      (32771 :ccitt-rle-word)      ; #1 w/ word alignment
      (32809 :thunderscan)         ; ThunderScan RLE
      (32773 :pack-bits)           ;  Macintosh RLE
      (32895 :it8-ct-pad)          ; IT8 CT w/padding 
      (32896 :it8-linework-rle)    ; IT8 Linework RLE 
      (32897 :it8-monochrome)      ; IT8 Monochrome picture 
      (32898 :it8-binary-line-art) ; IT8 Binary line art 
      (32908 :pixarfilm)           ; Pixar companded 10bit LZW 
      (32909 :pixarlog)            ; Pixar companded 11bit ZIP 
      (32946 :deflate)             ; Deflate compression 
      (32947 :dcs)                 ; Kodak DCS encoding 
      (34661 :jbig)                ; ISO JBIG 
      (34676 :sgi-log)             ; SGI Log Luminance RLE 
      (34677 :sgi-log24)           ; SGI Log 24-bit packed 
      (34712 :jp2000)))            ; Leadtools JPEG2000 

    (262 :photometric-interpretation 
     ((0 :white-is-zero)
      (1 :black-is-zero)
      (2 :rgb)
      (3 :rgb-palette)
      (4 :transparency-mask)
      (5 :cmyk)
      (6 :ycbcr)
      (8 :cie-lab)
      (9 :icc-lab)
      (10 :itu-lab)          ;; ITU L*a*b* 
      (32844 :cie-log2l)     ;; CIE Log2(L) 
      (32845 :cie-log2luv))) ;; CIE Log2(L) (u',v') 

    (263 :threshholding
     (1 :bilevel)        ;; b&w art scan 
     (2 :halftone)       ;; or dithered scan 
     (3 :error-diffuse)) ;; usually floyd-steinberg 

    (264 :cell-width)
    (265 :cell-length)

    (266 :fill-order 
     ((1 :msb2lsb)   ;; most significant -> least -- exiftool calls this normal
      (2 :lsb2msb))) ;; least significant -> most -- exiftool calls this reversed

    (269 :document-name)
    (270 :image-description)
    (271 :make)
    (272 :model)
    (273 :strip-offsets)

    (274 :orientation           ; SHORT
     ((1 :normal)               ; :top-left --  row 0 top, col 0 lhs 
      (2 :flipped-horizontally) ; :top-right -- row 0 top, col 0 rhs 
      (3 :rotated-180)          ; :bottom-right -- row 0 bottom, col 0 rhs 
      (4 :flipped-vertically)   ; :bottom-left --  row 0 bottom, col 0 lhs 
      (5 :rotated-270-and-flipped-horizontally) ; :left-top -- row 0 lhs, col 0 top 
      (6 :rotated-270)                  ; :right-top --  row 0 rhs, col 0 top 
      (7 :rotated-90-and-flipped-vertically) ; :right-bottom -- row 0 rhs, col 0 bottom 
      (8 :rotated-90)))               ; :left-bottom -- row 0 lhs, col 0 bottom 

    (277 :samples-per-pixel)
    (278 :rows-per-strip)
    (279 :strip-byte-counts)
    (280 :min-sample-value)
    (281 :max-sample-value)
    (282 :x-resolution)
    (283 :y-resolution)

    (284 :planar-configuration 
     ((1 :contiguous) ;; single image plane -- chunky
      (2 :separate))) ;; separate planes of data -- planar

    (285 :page-name)
    (286 :x-position)
    (287 :y-position)
    (288 :free-offsets)
    (289 :free-byte-counts)

    (290 :gray-response-unit
     ((1 :tenths)             
      (2 :hundredths)         
      (3 :thousandths)        
      (4 :ten-thousandths)    
      (5 :hundred-thousandths)))
    
    (291 :gray-response-curve)

    (292 :t4-options
     ((1 :2d-encoding)  ;; 2-dimensional coding 
      (2 :uncompressed) ;; data not compressed 
      (4 :fill-bits)))  ;; fill to byte boundary 

    (293 :t6-options
     (3 :uncompressed)) ;; data not compressed

    (296 :resolution-unit 
     ((1 nil)   ;; no meaningful units
      (2 :inch) ;; english
      ;; (3 :CENTIMETER)
      (3 :cm))) ;; metric

    (297 :page-number) ;; page numbers of multi-page :NOTE this is returned as an array.
    
    (300 :color-response-unit ;; color curve accuracy per unit
     ((1 :tenths)             
      (2 :hundredths)         
      (3 :thousandths)        
      (4 :ten-thousandths)    
      (5 :hundred-thousandths)))

    (301 :transfer-function) ;; colorimetry info
    (305 :software)          ;; name & release
    (306 :date-time)         ;; creation date and time
    (315 :artist)            ;; creator of image
    (316 :host-computer)     ;; machine where created

    (317 :predictor ;; prediction scheme w/ LZW
     ((1 nil)       ;; no prediction scheme use
      (2 :horizontal) ;; horizontal differencing  :NOTE exiftool reports "Horizontal differencing"
      (3 :floating-point))) ;; floating point predictor

    (318 :white-point) ;;  image-whitepoint
    (319 :primary-chromaticities)
    (320 :color-map)
    (321 :halftone-hints)
    (322 :tile-width)
    (323 :tile-length)
    (324 :tile-offsets)
    (325 :tile-byte-counts)
    (326 :bad-fax-lines) ;;  lines w/ wrong pixel count

    (327 :clean-fax-data ;; regenerated line info 
     ((0 :clean)         ;; no errors detected 
      (1 :regenerated)   ;; receiver regenerated lines 
      (2 :unclean)))     ;; uncorrected errors exist 

    (328 :max-bad-fax-lines) ;; max consecutive bad lines
    
    (330 :sub-ifds) ;; subimage descriptors - PageMaker extension
    
    (332 :ink-set      ;; inks in separated image 
     ((1 :cmyk)        ;; cyan-magenta-yellow-black color
      (2 :multi-ink))) ;; multi-ink or hi-fi color

    (333 :ink-names)
    (334 :number-of-inks)
    (336 :dot-range)
    (337 :target-printer)

    (338 :extra-samples
     ((0 :unspecified)          ;; unspecified data 
      (1 :associated-alpha)     ;; associated alpha data 
      (2 :unassociated-alpha))) ;; unassociated alpha data 

    (339 :sample-format
     ((1 :unisigned-integer)             ;; unsigned integer data 
      (2 :signed-integer)                ;; signed integer data 
      (3 :ieee-floating-point)           ;; IEEE floating point data 
      (4 :untyped)                       ;; untyped data 
      (5 :complex-signed-integer)        ;; complex signed int 
      (6 :complex-ieee-floating-point))) ;; complex ieee floating 

    (340 :s-min-sample-value)
    (341 :s-max-sample-value)
    (342 :transfer-range)
    (343 :clip-path)			; PageMaker extension
    (344 :x-clip-path-units)		; PageMaker extension
    (345 :y-clip-path-units)		; PageMaker extension
    (346 :indexed)			; PageMaker extension
    (347 :jpeg-tables)			; Photoshop extension
    (351 :opi-proxy)			; PageMaker extension
    
    (512 :JPEG-proc
     ((1 :baseline)    ;; baseline sequential 
      (14 :lossless))) ;; Huffman coded lossless 

    (513 :jpeg-interchange-format)
    (514 :jpeg-interchange-format-length)
    (515 :jpeg-restart-interval)
    (517 :jpeg-lossless-predictors)
    (518 :jpeg-point-transforms)
    (519 :jpeg-q-tables)
    (520 :jpeg-dc-tables)
    (521 :jpeg-ac-tables)

    ;; 4.5.3 Basic Structure of YCbCr Uncompressed Data
    (529 :ycbcr-coefficients) ;; RGB-YCbCr color transformation matrix coefficients
    (530 :ycbcr-sub-sampling) ;; Chrominance subsampling information
    (531 :ycbcr-positioning ;; short -- Information on matching/nonmatching of chrominance and luminance samples
     ((1 :centered)
      (2 :co-sited)))

    (532 :reference-black-white)
    (32781 :image-id)            ;; PageMaker extension
    (32954 :region-tack-point)	 ;; region-xform tack point 
    (32955 :region-warp-corners) ;; warp quadrilateral 
    (32956 :region-affine)	 ;; affine transformation mat 

    ;; tags 32995-32999 are private tags registered to SGI
    (32995 :matteing)    ;; use ExtraSamples 
    (32996 :data-type)   ;; use SampleFormat 
    (32997 :image-depth) ;; z depth of image 
    (32998 :tile-depth)  ;; z depth/data tile 

    ;; tags 33300-33309 are private tags registered to Pixar
    ;;
    ;; :pixar-image-full-width and :pixar-image-full-length
    ;; are set when an image has been cropped out of a larger image.  
    ;; They reflect the size of the original uncropped image.
    ;; The TIFFTAG_XPOSITION and TIFFTAG_YPOSITION can be used
    ;; to determine the position of the smaller image in the larger one.
    ;; 
    (33300 :pixar-image-full-width)  ;; full image size in x 
    (33301 :pixar-image-full-length) ;; full image size in y 
    ;; Tags 33302-33306 are used to identify special image modes and data
    ;; used by Pixar's texture formats.
    (33302 :pixar-texture-format) ;; texture map format 
    (33303 :pixar-wrap-modes)     ;; s & t wrap modes 
    (33304 :pixar-fov-cotan)      ;; cotan(fov) for env. maps 
    (33305 :pixar_matrix_world-to-screen)
    (33306 :pixar_matrix_world-to-camera)

    ;;  tag 33405 is a private tag registered to Eastman Kodak
    (33405 :writer-serial-number) ;;   device serial number

    ;; tag 33432 is listed in the 6.0 spec w/ unknown ownership
    (33432 :copyright)
    ;;
    ;; 34016-34029 are reserved for ANSI IT8 TIFF/IT <dkelly@apago.com) 
    ;;
    (34016 :it8-site)                          ;; site name 
    (34017 :it8-color-sequence)                ;; color seq. [rgb,cmyk,etc] 
    (34018 :it8-header)                        ;; ddes header 
    (34019 :it8-raster-padding)                ;; raster scanline padding 
    (34020 :it8-bits-per-run-length)           ;; # of bits in short run 
    (34021 :it8-bits-per-extended-run-length ) ;; # of bits in long run 
    (34022 :it8-colortable)                    ;; lw colortable 
    (34023 :it8-image-color-indicator)         ;; bp/bl image color switch 
    (34024 :it8-bk-gcolor-indicator)           ;; bp/bl bg color switch 
    (34025 :it8-image-color-value)             ;; bp/bl image color value 
    (34026 :it8-bk-gcolor-value)               ;; bp/bl bg color value 
    (34027 :it8-pixel-intensity-range)         ;; mp pixel intensity value 
    (34028 :it8-transparency-indicator)        ;; hc transparency switch 
    (34029 :it8-color-characterization)        ;; color character. table 
    (34030 :it8-hc-usage)                      ;; hc usage indicator 
    (34031 :it8-trap-indicator)  ;;trapping indicator (untrapped=0, trapped=1)
    (34032 :it8-cmyk-equivalent) ;; cmyk color equivalents 

    ;; tags 34232-34236 are private tags registered to Texas Instruments 
    (34232 :frame-count) ;; Sequence Frame Count 

    ;; tag 34377 is private tag registered to Adobe for PhotoShop 
    (34377 :photoshop)

    (34675 :icc-profile)  ;; ICC profile data
    (34750 :jbig-options) ;; JBIG options

    (34908 :fax-recv-params) ;; encoded class 2 ses. parms 
    (34909 :fax-sub-address) ;; received subaddr string 
    (34910 :fax-recv-time)   ;; receive time (secs) 
    (34911 :fax-dcs)         ;; encoded fax ses. params, Table 2/T.30 

    ;; tags 37439-37443 are registered to SGI <gregl@sgi.com>
    (37439 :sample-to-nits) ;; Sample value to Nits
    (34929 :fedex-edr) ;; tag 34929 is a private tag registered to FedEx with unknown use
    (37724 :image-source-data) ;; Photoshop extension
    
    (50341 :print-im) ;; Exif extension

    ;; Adobe Digital Negative (DNG) format tags
    (50706 :dng-version)               ; dNG version number 
    (50707 :dng-backward-version)      ; DNG compatibility version 
    (50708 :unique-camera-model)       ; name for the camera model 
    (50709 :localized-camera-model)    ; localized camera model name 
    (50710 :cfa-plane-color)           ; CFAPattern->LinearRaw space mapping 
    (50711 :cfa-layout)                ; spatial layout of the CFA 
    (50712 :linearization-table)       ; lookup table description 
    (50713 :black-level-repeat-dim)    ; repeat pattern size for the BlackLevel tag 
    (50714 :black-level)               ; zero light encoding level 
    (50715 :black-level-delta-h)       ; zero light encoding level differences (columns) 
    (50716 :black-level-delta-v)       ; zero light encoding level differences (rows) 
    (50717 :white-level)               ; fully saturated encoding level 
    (50718 :default-scale)             ; default scale factors 
    (50719 :default-crop-origin)       ; origin of the final image area 
    (50720 :default-crop-size)         ; size of the final image area
    (50721 :color-matrix-1)            ; xYZ->reference color space transformation matrix 1 
    (50722 :color-matrix-2)            ; XYZ->reference color space transformation matrix 2 
    (50723 :camera-calibration-1)      ; calibration matrix 1 
    (50724 :camera-calibration-2)      ; calibration matrix 2 
    (50725 :reduction-matrix-1)        ; dimensionality reduction matrix 1 
    (50726 :reductio-nmatrix-2)        ; dimensionality reduction matrix 2 
    (50727 :analog-balance)            ; gain applied the stored raw values
    (50728 :as-shot-neutral)           ; selected white balance in linear reference space 
    (50729 :as-shot-white-x-y)         ; selected white balance in x-y chromaticity coordinates 
    (50730 :baseline-exposure)         ; how much to move the zero point 
    (50731 :baseline-noise)            ; relative noise level 
    (50732 :baseline-sharpness)        ; relative amount of sharpening 
    (50733 :bayer-green-split)         ; how closely values of green pixels in the blue/green rows track values of green pixels in red/green rows
    (50734 :linear-response-limit)     ; non-linear encoding range 
    (50735 :camera-serial-number)      ; camera's serial number 
    (50736 :lens-info)                 ; info about the lens 
    (50737 :chroma-blur-radius)        ; chroma blur radius 
    (50738 :antialias-strength)        ; relative strength of the camera's anti-alias filter 
    (50739 :shadow0scale)              ; used by Adobe Camera Raw 
    (50740 :dng-private-data)          ; manufacturer's private data 
    (50741 :maker-note-safety)         ; whether the EXIF MakerNote tag is safe to preserve along with the rest of the EXIF data 
    (50778 :calibration-illuminant-1)  ; illuminant 1 
    (50779 :calibration-illuminant2)   ; illuminant 2 
    (50780 :best-quality-scale)        ; best quality multiplier 
    (50781 :raw-data-unique-id)        ; unique identifier for the raw image data 
    (50827 :original-raw-filename)     ; file name of the original raw file 
    (50828 :original-raw-filedata)     ; contents of the original raw file 
    (50829 :active-area)               ; active (non-masked) pixels of the sensor 
    (50830 :masked-areas)              ; list of coordinates of fully masked pixels 
    (50831 :as-shot-iccp-rofile)       ; these two tags used to 
    (50832 :as-shot-preprofile-matrix) ; map cameras's color space into ICC profile space 
    (50833 :current-icc-profile)         
    (50834 :current-pre-profile-matrix)

    ;; Exif extensions
    ;; tags 34665, 34853 and 40965 are documented in EXIF specification -- 4.6.3 Exif-specific IFD
    ;;
    (34665 :exif-ifd) ; `interpret-tag-value' -- Exif extension
    (34853 :gps-ifd)  ; `interpret-tag-value' -- Exif extension -- :NOTE zpb-exif-1.2.1/exif.lisp defines a tagset for GPS-ifd

    (33434 :exposure-time)   ; FLOATABLE-RATIONAL
    (33437 :f-number)        ; FLOATABLE-RATIONAL
    (34850 :exposure-program ; SHORT
     ((0 :not-defined)
      (1 :manual)
      (2 :normal-program)
      (3 :aperture-priority)
      (4 :shutter-priority)
      (5 :creative-program)
      (6 :action-program)
      (7 :portrait-mode)
      (8 :landscape-mode)))
    (34852 :spectral-sensitivity) ; ASCII

    (34855 :iso-speed-ratings) ; SHORT
    (34856 :oecf)              ;  Optoelectric conversion factor
    (36864 :exif-version)
    (36867 :date-time-original)  ; Date and time of original data generation
    (36868 :date-time-digitized) ; Date and time of digital data generation
    (37121 :components-configuration)  ; Meaning of each component
    (37122 :compressed-bits-per-pixel) ; Image compression mode
    (37377 :shutter-speed-value)       ; FLOATABLE-RATIONAL
    (37378 :aperture-value)            ; FLOATABLE-RATIONAL
    (37379 :brightness-value)          ; SIGNED-RATIONAL
    (37380 :exposure-bias-value)       ; SIGNED-RATIONAL
    (37381 :max-aperture-value) ; FLOATABLE-RATIONAL -- Maximum lens aperture
    (37382 :subject-distance)   ; FLOATABLE-RATIONAL -- Subject distance
    (37383 :metering-mode 
     ((0 :unknown) ;; nil)
      (1 :average)
      (2 :center-weighted-average)
      (3 :spot)
      (4 :multi-spot)
      (5 :pattern)
      (6 :partial)
      (255 :other)))
    (37384 :light-source 
     ((0  :unknown) ;; nil)
      (1  :daylight)
      (2  :fluorescent)
      (3  :tungsten)
      (4  :flash)
      (9  :fine-weather)
      (10 :cloudy-weather)
      (11 :shade)
      (12 :daylight-fluorescent)        ; (D 5700 - 7100K)
      (13 :day-white-fluorescent)       ; (N 4600 - 5400K)
      (14 :cool-white-fluorescent)      ; (W 3900 - 4500K)
      (15 :white-fluorescent)           ; (WW 3200 - 3700K)
      (17 :standard-light-A)
      (18 :standard-light-B)
      (19 :standard-light-C)
      (20 :d55)
      (21 :d65)
      (22 :d75)
      (23 :d50)
      (24 :iso-studio-tungsten)
      (255 :other-light-source)))

    (37385 :flash)        ;; SHORT -- `interpret-tag-value'

    (37386 :focal-length) ; FLOATABLE-RATIONAL
    (37396 :subject-area) ; SHORT
    (37500 :maker-note)   ; Manufacturer notes  :NOTE Nikon .nef files use this to hold proprietary raw data
    (37510 :user-comment)
    (37520 :sub-sec-time)           ; ASCII
    (37521 :sub-sec-time-original)  ; ASCII
    (37522 :sub-sec-time-digitized) ; ASCII
    
    (40960 :flashpix-version) ;; `interpret-tag-value' 
    
    (40961 :color-space ;; SHORT -- Color space information
     ((1     :srgb)
      (65535 :uncalibrated)))

    (40962 :pixel-x-dimension) ; Valid image width
    (40963 :pixel-y-dimension) ; valid image height
    (40964 :related-sound-file)

    (40965 :interoperability-IFD) ; `interpret-tag-value' -- 4.6.3 Exif-specific IFD

    (41483 :flash-energy) ; :RATIONAL
    (41484 :spatial-frequency-response) 
    (41486 :focal-plane-x-resolution)    ; FLOATABLE-RATIONAL
    (41487 :focal-plane-y-resolution)    ; FLOATABLE-RATIONAL
    (41488 :focal-plane-resolution-unit) ; SHORT
    (41492 :subject-location)            ; SHORT
    (41493 :exposure-index)              ; RATIONAL
    
    ;; (41495 :sensing-method 
    ;;  ((1 :undefined)
    ;;   (2 :one-chip-color-area)
    ;;   (3 :two-chip-color-area)
    ;;   (4 :three-chip-color-area)
    ;;   (5 :color-sequential-area)
    ;;   (7 :trilinear)
    ;;   (8 :color-sequential-linear)))

    ;; zpb-exif-1.2.1/exif.lisp
    (41495 :sensing-method
     ((1 :not-defined)
      (2 :one-chip-color-area-sensor)
      (3 :two-chip-color-area-sensor)
      (4 :three-chip-color-area-sensor)
      (5 :color-sequential-area-sensor)
      (6 :trilinear-sensor)
      (7 :color-sequential-linear-sensor)))
    
    (41728 :file-source 
     ((3 :dsc)))
    (41729 :scene-type 
     ((1 :direct)))
    (41730 :CFA-pattern)
    (41985 :custom-rendered ; SHORT
     ((0 nil)
      (1 t))) 
    (41986 :exposure-mode 
     ((0 :auto-exposure)
      (1 :manual-exposure)
      (2 :auto-bracket)))
    (41987 :white-balance  ; SHORT
     ((0 :auto)
      (1 :manual)))
    (41988 :digital-zoom-ratio ; RATIONAL
     ((0 nil)))
    (41989 :focal-length-in-35mm-film ; SHORT
     ((0 nil)))
    (41990 :scene-capture-type 
     ((0 :standard)
      (1 :landscape)
      (2 :portrait)
      (3 :night-scene)))
    (41991 :gain-control 
     ((0 :none)
      (1 :low-gain-up)
      (2 :high-gain-up)
      (3 :low-gain-down)
      (4 :high-gain-down)))
    (41992 :contrast 
     ((0 :normal)
      (1 :soft)
      (2 :hard)))
    (41993 :saturation 
     ((0 :normal)
      (1 :low-saturation)
      (2 :high-saturation)))
    (41994 :sharpness
     ((0 :normal)
      (1 :soft)
      (2 :hard)))
    (41995 :device-setting-description)
    (41996 :subject-distance-range
     ((0 :unknown)
      (1 :macro)
      (2 :close-view)
      (3 :distant-view)))
    (42016 :image-unique-id) ;; ascii
    ))

;;; ==============================
;;; EOF
