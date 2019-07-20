;;; image.scm --- Image packages

;; Copyright © 2015, 2017, 2019 Alex Kost <alezost@gmail.com>

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
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (gnu packages image-viewers)
  #:use-module (al guix utils)
  #:use-module (al places))

(define-public my-sxiv
  (package
    (inherit sxiv)
    (name "my-sxiv")
    ;; In version 25, the author decided to leave only 2 colors to
    ;; configure instead of 5:
    ;; <https://github.com/muennich/sxiv/commit/919ada11232781e3624363d834a6b9851c3a2bcb>.
    ;; Indeed, why do you need any customization at all?  The author
    ;; knows better what you want!  So I stick to version 24.
    (version "24")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/muennich/sxiv.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "020n1bdxbzqncprh8a4rnjzc4frp335yxbqh5w6dr970f7n5qm8d"))))
    (arguments
     `(#:tests? #f
       #:make-flags
       (list (string-append "PREFIX=" %output)
             "CC=gcc"
             ;; Because of the hand-written Makefile, my (cflags) overrides
             ;; the default CFLAGS (which are: "-std=c99 -Wall -pedantic").
             ,(cflags)
             ;; Xft.h #includes <ft2build.h> (without ‘freetype2/’).  The sxiv
             ;; Makefile works around this by hard-coding /usr/include instead.
             (string-append "DEF_CPPFLAGS=-I"
                            (assoc-ref %build-inputs "freetype")
                            "/include/freetype2")
             "V=1")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'add-custom-config.h
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((config.h (assoc-ref inputs "config.h")))
               (copy-file config.h "config.h")))))))
    (inputs
     (cons `("config.h" ,(local-file (config-file "sxiv/config.h")))
           (package-inputs sxiv)))
    (synopsis (string-append (package-synopsis sxiv)
                             " (compiled with custom 'config.h')"))))

;;; image.scm ends here
