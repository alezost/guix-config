;;; misc.scm --- Miscellaneous packages

;; Copyright © 2016–2017 Alex Kost

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
  #:use-module (gnu packages tv)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system trivial)
  #:use-module (al guix utils))

(define-public empty-package
  (package
    (name "empty-package")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (mkdir %output)))
    (synopsis "Empty package")
    (description
     "This package creates its store directory and nothing more.")
    (home-page #f)
    (license license:gpl3+)))

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

(define-public my-tvtime
  (package
    (inherit tvtime)
    (name "my-tvtime")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-mute
           ;; Along with killing/making the sound thread, 'tvtime' also
           ;; mutes/unmutes "Master" (or another used simple control) on
           ;; every channel switch.  This is redundant and it leads to
           ;; unpleasant results: for example, it may leave muted
           ;; "Master" on exit, also it always resets sound volume to
           ;; the value that was actual when tvtime was started.  So do
           ;; not mute the system, killing the sound thread is enough.
           (lambda _
             (substitute* "src/mixer.c"
               (("mixer->mute.*" all)
                (string-append "/* " all "*/\n"))))))
       ;; #:configure-flags (list "--disable-silent-rules")
       #:make-flags (list ,(cflags))))
    (synopsis (string-append (package-synopsis tvtime)
                             " (with proper mute/unmute behavior)"))))

;;; misc.scm ends here
