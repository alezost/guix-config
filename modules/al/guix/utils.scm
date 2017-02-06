;;; utils.scm --- Guix utilities

;; Copyright © 2015, 2017 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 26 Sep 2015

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

;; Various utilities for Guix.

;;; Code:

(define-module (al guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (guix profiles)
  #:export (lists-of-packages->manifest
            guix-package
            guix-packages
            my-package
            my-packages))

(define-syntax-rule (lists-of-packages->manifest packages ...)
  (packages->manifest
   (delete-duplicates (append packages ...) eq?)))

(define-syntax module-package
  (syntax-rules ()
    ((_ module (package output))
     (list (@ module package) output))
    ((_ module package)
     (@ module package))))

(define-syntax-rule (module-packages module package ...)
  (list (module-package module package)
        ...))

(define-syntax-rule (modules-packages (module package ...) ...)
  (append (module-packages module package ...)
          ...))

(define-syntax-rule (define-packages-macro macro-name module-part ...)
  (define-syntax macro-name
    (syntax-rules ::: ()
      ((_  (module-last-part package :::) :::)
       (modules-packages ((module-part ... module-last-part)
                          package :::)
                         :::)))))

(define-packages-macro guix-packages gnu packages)
(define-packages-macro my-packages al guix packages)

(define-syntax-rule (guix-package module-part package)
  "Return PACKAGE from (gnu packages MODULE-PART) module."
  (module-package (gnu packages module-part) package))

(define-syntax-rule (my-package module-part package)
  "Return PACKAGE from (al guix packages MODULE-PART) module."
  (module-package (al guix packages module-part) package))

;;; utils.scm ends here
