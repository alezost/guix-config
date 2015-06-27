;;; sxiv.scm --- Simple X Image Viewer

;; Copyright © 2015 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 25 Jun 2015

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; sxiv packages.

;;; Code:

(define-module (al guix packages sxiv)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (gnu packages sxiv)
  #:use-module (al places))

(define-public sxiv-git
  (let ((commit "9264a65"))
    (package
      (inherit sxiv)
      (name "sxiv-git")
      (version (string-append "1.3.1." commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "git://github.com/muennich/sxiv.git")
                      (commit commit)))
                (sha256
                 (base32
                  "0hbbricz7gdhrg93ar5h8i8w54dmb48c7kispslkhzc2r1vyb03z"))
                (file-name (string-append name "-" version)))))))

(define-public sxiv-configured
  (let ((sxiv sxiv-git))
    (package
      (inherit sxiv)
      (name "sxiv-configured")
      (arguments
       (substitute-keyword-arguments (package-arguments sxiv)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-before
              'build 'add-custom-config.h
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((config.h (assoc-ref inputs "config.h")))
                  (copy-file config.h "config.h"))))))))
      (inputs
       (cons `("config.h"
               ,(origin
                  (method url-fetch)
                  (uri (string-append "file://"
                                      (config-file "sxiv/config.h")))
                  (sha256
                   (base32
                    "1xz5n330mk6n43qd8a93bqi24mp9g4nmd4i78nk9yppjy1nji2ji"))))
             (package-inputs sxiv)))
      (synopsis (string-append (package-synopsis sxiv)
                               " (compiled with custom 'config.h')")))))

;;; sxiv.scm ends here
