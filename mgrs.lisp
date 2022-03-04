#|
Convert geographic coordinates between Latitude/Longitude and MGRS.

Copyright 2020-2022 Guillaume Le Vaillant

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(defpackage :mgrs
  (:use :common-lisp)
  (:import-from :utm-ups
                #:lat/lon->utm/ups
                #:utm->lat/lon
                #:ups->lat/lon)
  (:export #:lat/lon->mgrs
           #:mgrs->lat/lon))

(in-package :mgrs)


(defun letter (n)
  "Return the letter associated to number N."
  (check-type n (integer 0 23))
  (let ((letters (load-time-value "ABCDEFGHJKLMNPQRSTUVWXYZ" t)))
    (char letters n)))

(defun letter-position (c)
  "Return the number associated to letter C."
  (check-type c character)
  (let ((letters (load-time-value "ABCDEFGHJKLMNPQRSTUVWXYZ" t)))
    (position c letters :test #'char-equal)))

(defun minimum-northing (band)
  "Return the minimum northing of a UTM BAND."
  (case band
    ((#\C) 1100000)
    ((#\D) 2000000)
    ((#\E) 2800000)
    ((#\F) 3700000)
    ((#\G) 4600000)
    ((#\H) 5500000)
    ((#\J) 6400000)
    ((#\K) 7300000)
    ((#\L) 8200000)
    ((#\M) 9100000)
    ((#\N) 0)
    ((#\P) 800000)
    ((#\Q) 1700000)
    ((#\R) 2600000)
    ((#\S) 3500000)
    ((#\T) 4400000)
    ((#\U) 5300000)
    ((#\V) 6200000)
    ((#\W) 7000000)
    ((#\X) 7900000)
    (t (error "Invalid band: ~a" band))))

(defun utm->mgrs (zone band easting northing precision)
  "Return the MGRS locator for the given UTM coordinates."
  (declare (optimize (debug 3)))
  (check-type zone (or (integer -60 -1) (integer 1 60)))
  (check-type band character)
  (check-type easting (real 0 1000000))
  (check-type northing (real 0 10000000))
  (check-type precision (integer 1 10000))
  (let* ((zone (abs zone))
         (northing-offset (if (oddp zone) 0 500000))
         (row (floor (mod (+ northing northing-offset) 2000000) 100000))
         (column (1- (floor easting 100000)))
         (square-x (letter (+ (* 8 (mod (1- zone) 3)) column)))
         (square-y (letter row))
         (p (case precision
              ((1) 5)
              ((10) 4)
              ((100) 3)
              ((1000) 2)
              ((10000) 1)
              (t (error "Invalid precision: ~a" precision))))
         (easting (floor (mod easting 100000) precision))
         (northing (floor (mod northing 100000) precision)))
    (format nil "~d~c~c~c~v,'0d~v,'0d"
            zone band square-x square-y p easting p northing)))

(defun mgrs/utm->lat/lon (zone band square-x square-y easting northing)
  "Return tle latitude and longitude for the given MGRS coordinates."
  (check-type zone (or (integer -60 -1) (integer 1 60)))
  (check-type band character)
  (check-type square-x (integer 0 23))
  (check-type square-y (integer 0 23))
  (check-type easting (integer 0 99999))
  (check-type northing (integer 0 99999))
  (let* ((square-x (- square-x (* 8 (mod (1- zone) 3))))
         (square-y (if (oddp zone) square-y (- square-y 5)))
         (zone (if (char>= band #\N) zone (- zone)))
         (easting (+ (* (1+ square-x) 100000) easting))
         (minimum-northing (minimum-northing band))
         (northing (loop for n from (+ (* square-y 100000) northing) by 2000000
                         while (< n minimum-northing)
                         finally (return n))))
    (utm->lat/lon zone easting northing)))

(defun false-easting (band)
  "Return the false easting for a UPS BAND."
  (check-type band character)
  (case band
    ((#\A #\Y) 800000)
    ((#\B #\Z) 2000000)
    (t (error "Invalid band: ~a" band))))

(defun false-northing (band)
  "Return the false northing for a UPS BAND."
  (check-type band character)
  (case band
    ((#\A #\B) 800000)
    ((#\Y #\Z) 1300000)
    (t (error "Invalid band: ~a" band))))

(defun ups->mgrs (northp band easting northing precision)
  "Return the MGRS locator for the given UPS coordinates."
  (declare (ignore northp))
  (check-type band character)
  (check-type easting (real 0 4000000))
  (check-type northing (real 0 4000000))
  (check-type precision (integer 1 10000))
  (let* ((row (floor (- northing (false-northing band)) 100000))
         (column (floor (- easting (false-easting band)) 100000))
         (column (if (or (char= band #\A) (char= band #\Y))
                     (cond
                       ((> column 8) (+ column 12))
                       ((> column 2) (+ column 10))
                       (t (+ column 8)))
                     (cond
                       ((> column 8) (+ column 4))
                       ((> column 2) (+ column 2))
                       (t column))))
         (square-x (letter column))
         (square-y (letter row))
         (p (case precision
              ((1) 5)
              ((10) 4)
              ((100) 3)
              ((1000) 2)
              ((10000) 1)
              (t (error "Invalid precision: ~a" precision))))
         (easting (floor (mod easting 100000) precision))
         (northing (floor (mod northing 100000) precision)))
    (format nil "~c~c~c~v,'0d~v,'0d"
            band square-x square-y p easting p northing)))

(defun mgrs/ups->lat/lon (northp band square-x square-y easting northing)
  "Return tle latitude and longitude for the given MGRS coordinates."
  (check-type northp boolean)
  (check-type band character)
  (check-type square-x (integer 0 23))
  (check-type square-y (integer 0 23))
  (check-type easting (integer 0 99999))
  (check-type northing (integer 0 99999))
  (let* ((square-x (if (or (char= band #\A) (char= band #\Y))
                       (cond
                         ((> square-x 20) (- square-x 12))
                         ((> square-x 12) (- square-x 10))
                         (t (- square-x 8)))
                       (cond
                         ((> square-x 12) (- square-x 4))
                         ((> square-x 4) (- square-x 2))
                         (t square-x))))
         (easting (+ (* square-x 100000) (false-easting band) easting))
         (northing (+ (* square-y 100000) (false-northing band) northing)))
    (ups->lat/lon northp easting northing)))

(defun lat/lon->mgrs (latitude longitude &optional (precision 1))
  "Return the MGRS locator for the given LATITUDE and LONGITUDE. The PRECISION
(in meters) of the locator can be 1, 10, 100, 1000, 10000."
  (check-type latitude (real -90 90))
  (check-type longitude (real -180 (180)))
  (check-type precision (integer 1 10000))
  (multiple-value-bind (utm/ups band) (lat/lon->utm/ups latitude longitude)
    (destructuring-bind (zone easting northing) utm/ups
      (let ((band (char-upcase (char band 0))))
        (case band
          ((#\A #\B #\Y #\Z)
           (ups->mgrs zone band easting northing precision))
          (t
           (utm->mgrs zone band easting northing precision)))))))

(defun mgrs->lat/lon (locator &optional center-p)
  "Return the latitude and longitude for the southwest corner of the given
MGRS LOCATOR square, or the center of the square if CENTER-P is not NIL."
  (check-type locator string)
  (let ((size (length locator)))
    (multiple-value-bind (zone band-i) (parse-integer locator :junk-allowed t)
      (unless (or (and (integerp zone)
                       (<= 1 zone 60)
                       (> size 3)
                       (> size band-i)
                       (evenp (- size band-i 1)))
                  (and (null zone)
                       (> size 1)
                       (= band-i 0)
                       (evenp (- size band-i 1))))
        (error "Invalid locator: ~a" locator))
      (let* ((square-x-i (1+ band-i))
             (square-y-i (1+ square-x-i))
             (easting-i (1+ square-y-i))
             (northing-i (+ easting-i (/ (- size easting-i) 2)))
             (precision (case (- size northing-i)
                          ((1) 10000)
                          ((2) 1000)
                          ((3) 100)
                          ((4) 10)
                          ((5) 1)
                          (t (error "Invalid locator: ~a" locator))))
             (band (char-upcase (char locator band-i)))
             (square-x (if (> size square-x-i)
                           (letter-position (char locator square-x-i))
                           0))
             (square-y (if (> size square-y-i)
                           (letter-position (char locator square-y-i))
                           0))
             (easting (if (> size northing-i)
                          (parse-integer locator
                                         :start easting-i
                                         :end northing-i)
                          0))
             (northing (if (> size northing-i)
                           (parse-integer locator :start northing-i)
                           0)))
        (unless (and square-x square-y easting northing)
          (error "Invalid locator: ~a" locator))
        (let ((easting (if center-p
                           (+ (* easting precision) (floor precision 2))
                           (* easting precision)))
              (northing (if center-p
                            (+ (* northing precision) (floor precision 2))
                            (* northing precision))))
          (case band
            ((#\A #\B)
             (mgrs/ups->lat/lon nil band square-x square-y easting northing))
            ((#\Y #\Z)
             (mgrs/ups->lat/lon t band square-x square-y easting northing))
            (t
             (mgrs/utm->lat/lon zone band square-x square-y easting northing))))))))
