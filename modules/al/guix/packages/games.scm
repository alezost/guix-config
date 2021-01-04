;;; games.scm --- Game packages

;; Copyright © 2019–2021 Alex Kost <alezost@gmail.com>

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

;; Games and other packages related to games.

;;; Code:

(define-module (al guix packages games)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sdl)
  #:use-module ((guix licenses) #:prefix license:)
  ;; #:use-module (al guix utils)  ; for cflags
  )

(define-public fheroes2
  ;; This package compiles and installs "fheroes2" binary.  To make the
  ;; whole game work, you need to:
  ;;
  ;; - make a directory, where your fheroes2 game will live (for
  ;;   example, "~/.fheroes2" or "~/games/fheroes2"); this directory
  ;;   will be called <dir> below;
  ;;
  ;; - install this package (to your guix profile);
  ;;
  ;; - make a symlink of "<guix-profile>/bin/fheroes2" to <dir>;
  ;;
  ;; - copy the following files from fheroes2 source code to <dir>:
  ;;   "fheroes2.cfg", "fheroes2.key" (these files are your settings)
  ;;   and optionally (not necessary to run the game) "files".
  ;;
  ;; - copy (or make symlinks of) the following directories from the
  ;;   original Heroes II game to <dir>: "data" and "maps" (note, they
  ;;   should be renamed from "DATA" and "MAPS" to the lower-case).
  ;;
  ;; Now you can start the game with "cd <dir>; ./fheroes2".

  (package
    (name "fheroes2")
    (version "0.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ihhub/fheroes2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ynf6d6gz58qjhij08465kfy77cq44jy8sfak8s6qwyjbs89yan4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags
       (list
        "WITH_AI=simple"
        ;; Unfortunately, I cannot use my CFLAGS because the
        ;; hand-written Makefiles use this ^^^^ environment
        ;; variable heavily (instead of leaving it for users).
        ;;
        ;; ,(cflags)
        (string-append "CC=" ,(cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append (assoc-ref inputs "sdl-union")
                                    "/include/SDL:"
                                    (or (getenv "CPATH") "")))
             #t))
         (delete 'configure)  ; no "configure", just "Makefile"
         (replace 'install ; no "install" target in the hand-written Makefile
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "src/dist/fheroes2" bin)
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)))
    (inputs
     `(("freetype" ,freetype)
       ("sdl-union" ,(sdl-union (list sdl
                                      sdl-image
                                      sdl-mixer
                                      sdl-net
                                      sdl-ttf)))))
    (home-page "https://github.com/ihhub/fheroes2")
    (synopsis "Free Heroes2 Engine")
    (description "@code{Free Heroes2} is an engine recreation of the
game @code{Heroes of Might and Magic II}.")
    (license license:gpl2+)))

;;; games.scm ends here
