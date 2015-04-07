;;; x.scm --- X server and related services

;; Copyright © 2015 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created:  3 Apr 2015

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

;; Various services related to the X Window System.

;;; Code:

(define-module (al guix services x)
  #:use-module (gnu services)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (gnu packages xorg)
  #:export (xorg-service))

(define* (xorg-service #:key config-dir (display ":0") (vt "vt7")
                       (modules (list xf86-video-vesa xf86-input-evdev))
                       (extra-options '("-nolisten" "tcp"
                                        "-logverbose" "-noreset")))
  "Start Xorg server on DISPLAY using virtual terminal VT.

CONFIG-DIR is a configuration directory (aka 'xorg.conf.d').  It should
be specified.

MODULES is a list of packages that provide appropriate libraries in
'lib/xorg/modules' subdirectories.

EXTRA-OPTIONS is a list of additional command-line arguments passed to
Xorg command."
  (let ((modules (cons xorg-server modules)))
    (with-monad %store-monad
      (return
       (service
        (documentation (format #f "Xorg server (display ~a)" display))
        (provision (list (symbol-append 'x (string->symbol display))))
        (requirement '(user-processes udev))
        (start
         #~(make-forkexec-constructor
            (append (list (string-append #$xorg-server "/bin/X")
                          #$display #$vt
                          "-configdir" #$config-dir
                          ;; Concatenate modules with commas.
                          "-modulepath"
                          (format #f "~{~a/lib/xorg/modules~^,~}"
                                  '#$modules))
                    '#$extra-options)))
        (stop #~(make-kill-destructor))
        (respawn? #f))))))

;;; x.scm ends here
