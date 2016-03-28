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
  #:use-module (gnu packages bash)
  #:use-module (gnu packages xorg))

(define-public xdaemon
  (package
    (name "xdaemon")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/alezost/xdaemon/releases/download/v"
                    version "/xdaemon-" version ".tar.gz"))
              (sha256
               (base32
                "1skga4kq5zhw26ah2mxl4l68gxcf1m7dz75iywsdizw002bcypdg"))))
    (build-system gnu-build-system)
    (inputs
     `(("bash" ,bash)
       ("xorg-server" ,xorg-server)))
    (home-page "https://github.com/alezost/xdaemon")
    (synopsis "Run X server as a daemon")
    (description
     "Xdaemon is a wrapper bash script that allows to turn X server into
a daemon.  When Xdaemon is started, it runs Xorg server, then waits
until it will be ready to accept connections from clients, and quits.
Another script that comes with this package is Xkill.  It allows a user
to kill an X server running on a particular @code{DISPLAY}.")
    ;; 'Xdaemon' script is under FreeBSD, the rest is under GPL3 or later.
    (license (list license:bsd-2 license:gpl3+))))

(define-public xdpyprobe
  (package
    (name "xdpyprobe")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/alezost/xdpyprobe/releases/download/v"
                    version "/" name "-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h09wd2qcg08rj5hcakvdh9q01hkrj8vxly94ax3ch2x06lm0zq8"))))
    (build-system gnu-build-system)
    (inputs
     `(("libx11" ,libx11)))
    (home-page "https://github.com/alezost/xdpyprobe")
    (synopsis "Probe X server for connectivity")
    (description
     "Xdpyprobe is a tiny C program whose only purpose is to probe a
connectivity of the X server running on a particular @code{DISPLAY}.")
    (license license:gpl3+)))

;;; x.scm ends here
