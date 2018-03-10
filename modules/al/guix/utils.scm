;;; utils.scm --- Guix utilities

;; Copyright © 2015, 2017, 2018 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 26 Sep 2015

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

;; Various utilities for Guix.

;;; Code:

(define-module (al guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (gnu packages)
  #:use-module (al utils)
  #:export (lists-of-packages->manifest
            guix-package
            guix-packages
            my-package
            my-packages
            specifications->packages
            remove-packages
            cflags))

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

(define (spec->package spec)
  "Like `specification->package' but better."
  (let-values (((package output)
                (specification->package+output spec)))
    (match output
      ("out" package)
      (_ (list package output)))))

(define-syntax-rule (specifications->packages spec ...)
  "Return PACKAGES matching SPEC specifications.
This is a plural form of `specification->package'."
  (map spec->package (list spec ...)))

(define (remove-packages names-or-packages packages)
  "Remove NAMES-OR-PACKAGES from PACKAGES."

  (define (name-or-package->proc name-or-package)
    "Return a function to define if a package matches NAME-OR-PACKAGE."
    (match name-or-package
      ((? string? name)
       (lambda (pkg)
         (string=? name (package-name pkg))))
      ((? package? package)
       (lambda (pkg)
         (eq? package pkg)))
      (_ (const #f))))

  (define (reduce-packages matching? packages)
    "Remove the first (MATCHING? package) from PACKAGES."
    (let loop ((checked '())
               (unchecked packages))
      (if (null? unchecked)
          checked
          (match unchecked
            ((current . rest)
             (if (matching? current)
                 (append checked rest)
                 (loop (cons current checked)
                       rest)))))))

  (let loop ((names-or-packages names-or-packages)
             (packages packages))
    (if (or (null? packages)
            (null? names-or-packages))
        packages
        (let ((matching? (name-or-package->proc
                          (car names-or-packages))))
          (loop (cdr names-or-packages)
                (reduce-packages matching? packages))))))

(define* (cflags #:key (main-flags '("-O2" "-march=native"))
                       (extra-flags '()))
  "Return 'CFLAGS=...' string."
  (string-append "CFLAGS="
                 (mapconcat identity
                            (append main-flags extra-flags)
                            " ")))

;;; utils.scm ends here
