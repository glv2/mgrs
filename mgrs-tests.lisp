#|
Convert geographic coordinates between Latitude/Longitude and MGRS.

Copyright 2020 Guillaume LE VAILLANT

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

(defpackage :mgrs-tests
  (:use :common-lisp :fiveam :mgrs))

(in-package :mgrs-tests)


(def-suite mgrs-tests)

(in-suite mgrs-tests)

(defconstant +max-error-rate+ 0.0001)

(defun close-enough (x y)
  (<= (- 1 +max-error-rate+) (abs (/ x y)) (+ 1 +max-error-rate+)))

(defparameter *mgrs-data*
  '(((43.642567d0 -79.38714d0) "17TPJ3008433438")
    ((57.15d0 -2.15d0) "30VWJ5142634403")
    ((-34.916666d0 138.6d0) "54HTG8075133569")
    ((13.75d0 100.5d0) "47PPR6217620582")
    ((-34.583338d0 -58.36667d0) "21HUB7465572312")
    ((40.433333d0 -3.7d0) "30TVK4062776089")
    ((-77.846389d0 166.668333d0) "58CEU3920458215")
    ((36.165926d0 -86.723285d0) "16SEF2488702387")
    ((-33.014673d0 116.230695d0) "50HMJ2814546823")
    ((-55.315349d0 -68.794971d0) "19FEU1301270096")
    ((35.205535d0 136.56579d0) "53SPU4252896959")
    ((-88 -1) "AZQ9612422035")
    ((-87 3) "BAR1743532687")
    ((85 10) "ZAB9645452981")
    ((88 -12) "YZE5382982783")
    ((86 50) "ZFE4032914429")
    ((86 -50) "YUE5967014429")
    ((-86 50) "BFQ4032985570")
    ((-86 -50) "AUQ5967085570")
    ((87 130) "ZCK5520314141")
    ((87 -130) "YXK4479614141")
    ((-87 130) "BCK5520385858")
    ((-87 -130) "AXK4479685858")))

(test lat/lon->mgrs
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (let* ((locator (second data))
                   (mgrs (lat/lon->mgrs latitude longitude)))
              (is (string= locator mgrs)))))
        *mgrs-data*))

(test mgrs->lat/lon
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (let ((locator (second data)))
              (destructuring-bind (lat lon) (mgrs->lat/lon locator)
                (is (close-enough latitude lat))
                (is (close-enough longitude lon))))))
        *mgrs-data*))
