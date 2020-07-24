;;; games.scm --- Game packages

;; Copyright © 2019–2020 Alex Kost <alezost@gmail.com>

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
  #:use-module (guix svn-download)
  #:use-module (guix build-system gnu)
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

  (let ((svn-revision 3279))    ; there are no releases
    (package
      (name "fheroes2")
      (version (string-append "r" (number->string svn-revision)))
      (source
       (origin
         (method svn-fetch)
         (uri (svn-reference
               (url "http://svn.code.sf.net/p/fheroes2/code/trunk")
               (revision svn-revision)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0mq53a9algf8zrlfvaj9q99qwwx0fq4r687c3iqb3x74xhbzy8ws"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
         #:make-flags
         (list
          ;; Actually, setting WITH_AI here does not work because
          ;; "empty" AI is hardcoded inside Makefile (fixed by
          ;; 'patch-Makefile' phase).
          "WITH_AI=simple"
          ;; Unfortunately, I cannot use my CFLAGS because the
          ;; hand-written Makefiles use this ^^^^ environment
          ;; variable heavily (instead of leaving it for users).
          ;;
          ;; ,(cflags)
          )
         #:phases
         (modify-phases %standard-phases
           (add-after 'set-paths 'set-sdl-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "CPATH"
                       (string-append (assoc-ref inputs "sdl-union")
                                      "/include/SDL:"
                                      (or (getenv "CPATH") "")))
               #t))
           (add-after 'unpack 'enter-fheroes2-dir
             (lambda _ (chdir "fheroes2") #t))
           (add-after 'enter-fheroes2-dir 'patch-Makefile
             (lambda _
               (substitute* "Makefile"
                 (("WITH_AI=empty") "WITH_AI=simple"))
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
      (home-page "http://sourceforge.net/projects/fheroes2")
      (synopsis "Free Heroes2 Engine")
      (description "@code{Free Heroes2} is an engine recreation of the
game @code{Heroes of Might and Magic II}.")
      (license license:gpl2+))))

;;; games.scm ends here
