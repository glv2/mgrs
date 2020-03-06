#|
Convert geographic coordinates between Latitude/Longitude and MGRS.

Copyright 2020 Guillaume Le Vaillant

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

(asdf:defsystem "mgrs"
  :name "mgrs"
  :description "Convert coordinates between Latitude/Longitude and MGRS."
  :version "1.0"
  :author "Guillaume Le Vaillant"
  :license "GPL-3"
  :depends-on ("utm-ups")
  :in-order-to ((test-op (test-op "mgrs/tests")))
  :components ((:file "mgrs")))

(asdf:defsystem "mgrs/tests"
  :name "mgrs/tests"
  :description "Unit tests for mgrs"
  :version "1.0"
  :author "Guillaume Le Vaillant"
  :license "GPL-3"
  :depends-on ("fiveam" "mgrs")
  :in-order-to ((test-op (load-op "mgrs/tests")))
  :perform (test-op (o s)
             (let ((tests (uiop:find-symbol* 'mgrs-tests :mgrs-tests)))
               (uiop:symbol-call :fiveam 'run! tests)))
  :components ((:file "mgrs-tests")))
