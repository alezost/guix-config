;;; fonts.scm --- Font packages

;; Copyright © 2014, 2015, 2017–2018, 2020 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 22 Oct 2014

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

;; Various font packages.

;;; Code:

(define-module (al guix packages fonts)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages compression))

(define-public font-alias-pure
  (package
    (inherit font-alias)
    (name "font-alias-pure")
    (arguments '())
    (synopsis (string-append (package-synopsis font-alias)
                             " (upstream package without modifications)"))))

(define-public font-symbola
  (package
    (name "font-symbola")
    (version "13.00")
    (source (origin
              (method url-fetch)
              (uri "https://dn-works.com/wp-content/uploads/2020/UFAS-Fonts/Symbola.zip")
              (sha256
               (base32
                "101nk6aar67zlqqbzskggbiybjmq4p7d8jc0vnsi789bz67ls0fs"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))

         (let ((unzip    (string-append (assoc-ref %build-inputs "unzip")
                                        "/bin/unzip"))
               (font-dir (string-append %output "/share/fonts/opentype"))
               (doc-dir  (string-append %output "/share/doc/" ,name)))
           (system* unzip (assoc-ref %build-inputs "source"))
           (mkdir-p font-dir)
           (mkdir-p doc-dir)
           (for-each (lambda (font)
                       (copy-file font
                                  (string-append font-dir "/"
                                                 (basename font))))
                     (find-files "." "\\.otf$"))
           (for-each (lambda (doc)
                       (copy-file doc
                                  (string-append doc-dir "/"
                                                 (basename doc))))
                     (find-files "." "\\.pdf$"))))))
    (native-inputs
     `(("source" ,source)
       ("unzip" ,unzip)))
    (home-page "https://dn-works.com/ufas/")
    (synopsis "Font with many Unicode symbols")
    (description
     "Symbola is a TrueType font providing basic Latin, Greek, Cyrillic and many
Symbol blocks of Unicode.")

    ;; The license is vague; that's why Symbola font is not a part of
    ;; Guix.  See:
    ;; <http://lists.gnu.org/archive/html/guix-devel/2014-10/msg00283.html>,
    ;; <http://lists.gnu.org/archive/html/guix-devel/2015-01/msg00200.html>.

    ;; All we have is the following cite from the home page:

    ;; In lieu of a licence: Fonts and documents in this site are not
    ;; pieces of property or merchandise items; they carry no
    ;; trademark, copyright, license or other market tags; they are
    ;; free for any use.
    (license license:public-domain)))
