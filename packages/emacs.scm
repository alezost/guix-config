;;; emacs.scm --- Emacs packages

;; Copyright © 2015 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 19 Jul 2015

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

;; Various emacs packages.

;;; Code:

(define-module (al guix packages emacs)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages mp3))

(define-public emms-minimal
  (package
    (inherit emms)
    (name "emms-minimal")
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" %output)
             "CC=gcc")
       ,@(substitute-keyword-arguments (package-arguments emms)
           ((#:phases phases)
            `(modify-phases ,phases
               (replace 'configure
                 (lambda _ (setenv "SHELL" (which "sh")))))))))
    (inputs `(("taglib" ,taglib)))
    (synopsis (string-append (package-synopsis emms)
                             " (without extra dependencies)"))))

;;; emacs.scm ends here
