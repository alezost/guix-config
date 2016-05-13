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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages mp3))

(define-public emacs-emms-minimal
  (package
    (inherit emms)
    (name "emacs-emms-minimal")
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

(define-public emacs-magit-minimal
  (package
    (inherit magit)
    (name "emacs-magit-minimal")
    (arguments
     (substitute-keyword-arguments (package-arguments magit)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'patch-exec-paths
             (lambda _
               (with-directory-excursion "lisp"
                 (emacs-substitute-variables "magit-git.el"
                   ("magit-git-executable" "git"))
                 (emacs-substitute-variables "magit-process.el"
                   ;; This crap tries to run "git" to define the
                   ;; default value (which is nil anyway).
                   ("magit-need-cygwin-noglob" '()))
                 #t)))))))
    (inputs '())
    (synopsis (string-append (package-synopsis magit)
                             " (without git dependency)"))))

(define-public emacs-w3m-minimal
  (package
    (inherit emacs-w3m)
    (name "emacs-w3m-minimal")
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-w3m)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'patch-exec-paths
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (emacs-substitute-variables "w3m.el"
                   ("w3m-icon-directory"
                    (string-append out "/share/images/emacs-w3m")))
                 #t)))))))
    (inputs '())
    (native-inputs
     `(("autoconf" ,autoconf)
       ("emacs" ,emacs-no-x)))
    (synopsis (string-append (package-synopsis emacs-w3m)
                             " (without extra dependencies)"))))

(define-public emacs-wget-minimal
  (package
    (inherit emacs-wget)
    (name "emacs-wget-minimal")
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-wget)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'patch-exec-paths)))))
    (inputs '())
    (native-inputs
     `(("emacs" ,emacs-no-x)))
    (synopsis (string-append (package-synopsis emacs-wget)
                             " (without wget dependencies)"))))

;;; emacs.scm ends here
