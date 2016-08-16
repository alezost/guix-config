;;; misc.scm --- Miscellaneous packages

;; Copyright © 2016 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 13 Aug 2016

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Various packages not suitable for the other modules.

;;; Code:

(define-module (al guix packages misc)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system trivial))

(define-public suspend
  (package
    (name "suspend")
    (version "0.1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((bin-dir (string-append %output "/bin"))
                (file    (string-append bin-dir "/suspend")))
           (mkdir-p bin-dir)
           (call-with-output-file file
             (lambda (port)
               (format port "\
#!/bin/sh
loginctl suspend
")))
           (chmod file #o555)))))
    (home-page #f)
    (synopsis "Shell script to suspend a system")
    (description "Suspend is a simple wrapper to run 'loginctl suspend'.")
    (license license:gpl3+)))

;;; misc.scm ends here
