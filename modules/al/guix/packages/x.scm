;;; x.scm --- Packages related to X server

;; Copyright © 2016–2017 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 22 Feb 2016

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

;; Various packages related to the X Window System.

;;; Code:

(define-module (al guix packages x)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages xorg)
  #:use-module (al guix utils))

(define-public my-libxfont2
  (package
    (inherit libxfont2)
    (name "my-libxfont")
    ;; XXX Remove 'version' and 'source' after 'libxfont2' package will
    ;; be switched to 2.0.3.
    (version "2.0.3")
    (source
     (origin
       (inherit (package-source libxfont2))
       (uri (string-append "mirror://xorg/individual/lib/libXfont2-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "0klwmimmhm3axpj8pwn5l41lbggh47r5aazhw63zxkbwfgyvg2hf"))))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-fonts-can-be-links
           (lambda _
             ;; Fix the problem introduced by
             ;; <https://cgit.freedesktop.org/xorg/lib/libXfont/commit/?id=7b377456f95d2ec3ead40f4fb74ea620191f88c8>:
             ;; if "fonts.dir" is a symlink, do not ignore it as all
             ;; files in Guix profiles are symlinks!
             (substitute* '("src/fontfile/dirfile.c"
                            "src/fontfile/fileio.c")
               (("\\| ?O_NOFOLLOW") "")))))))
    (synopsis (string-append (package-synopsis libxfont2)
                             " (with fixed fonts determination)"))))

(define-public my-xorg-server
  (package
    (inherit xorg-server)
    (name "my-xorg-server")
    (inputs (append `(("libxfont2" ,my-libxfont2))
                    (alist-delete "libxfont2"
                                  (package-inputs xorg-server))))
    (arguments
     (append `(#:make-flags (list ,(cflags)))
             (package-arguments xorg-server)))
    (synopsis (string-append (package-synopsis xorg-server)
                             " (with my customizations)"))))

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
       ("xorg-server" ,my-xorg-server)))
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

;;; x.scm ends here
