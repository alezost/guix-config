;;; x.scm --- Packages related to X server

;; Copyright © 2016 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 22 Feb 2016

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

;; Various packages related to the X Window System.

;;; Code:

(define-module (al guix packages x)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages xorg))

(define-public xdaemon
  (package
    (name "xdaemon")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/alezost/xdaemon/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1k2cg6x1jzcpx1mq871ic04i5flg155f5rpn8jgmaj7agp8hibfx"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bootstrap
           (lambda _ (zero? (system* "autoreconf" "-vfi")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("bash" ,bash)
       ("xorg-server" ,xorg-server)))
    (home-page "https://github.com/alezost/xdaemon")
    (synopsis "Run X server as a daemon")
    (description
     "Xdaemon is a wrapper bash script that allows to turn X server into
a daemon.  When Xdaemon is started, it runs Xorg server, then waits
until it will be ready to accept connections from clients, and quits.
Another script that comes with this package is Xkill.  It allows to kill
an X server run on a particular @code{DISPLAY}.")
    ;; 'Xdaemon' script is under FreeBSD, the rest is under GPL3 or later.
    (license (list license:bsd-2 license:gpl3+))))

;;; x.scm ends here
