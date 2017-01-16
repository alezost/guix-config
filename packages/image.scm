;;; image.scm --- Image packages

;; Copyright © 2015, 2017 Alex Kost <alezost@gmail.com>

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

;; Image viewers and other image related packages.

;;; Code:

(define-module (al guix packages image)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (gnu packages image-viewers)
  #:use-module (al places))

(define-public my-sxiv
  (package
    (inherit sxiv)
    (name "my-sxiv")
    (arguments
     (substitute-keyword-arguments (package-arguments sxiv)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'build 'add-custom-config.h
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((config.h (assoc-ref inputs "config.h")))
                 (copy-file config.h "config.h"))))))))
    (inputs
     (cons `("config.h" ,(local-file (config-file "sxiv/config.h")))
           (package-inputs sxiv)))
    (synopsis (string-append (package-synopsis sxiv)
                             " (compiled with custom 'config.h')"))))

;;; image.scm ends here
