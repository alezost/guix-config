#!/usr/bin/guile \
-e main -s
!#
;;; profile.scm --- Populate my Guix profiles

;; Copyright © 2015, 2016 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 30 Nov 2015

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

;; Just a little script to reduce writing for populating my profiles
;; using manifest files.

;;; Code:

(use-modules
 (ice-9 format)
 (ice-9 ftw)
 (ice-9 match)
 (ice-9 regex)
 (srfi srfi-1)
 (srfi srfi-26)
 (al places))

(define (show-help)
  (format #t "Usage: ~a [OPTION] NAME [ARGS ...]
Populate profile with manifest file using the following command:

  guix package --profile='~a' --manifest='~a' ARGS ..."
          (car (command-line))
          (guix-profile-file "NAME")
          (guix-manifest-file "NAME"))
  (display "

NAME may also be 'all' which means populate profiles with all available
manifests.

Options:
  -h, --help        display this help and exit
  -l, --list        list available NAMEs and exit")
  (newline))

(define (names)
  "Return a list of available profile names."
  (let ((file-names (scandir (guix-manifest-file)))
        (rx (make-regexp "\\`manifest-(.*)\\.scm\\'")))
    (filter-map (lambda (file-name)
                  (and=> (regexp-exec rx file-name)
                         (cut match:substring <> 1)))
                file-names)))

(define (display-names names)
  (display "Available profile names:\n")
  (format #t "~{~a~%~}" (sort names string-ci<)))

(define (populate-profile name . rest-args)
  (let ((profile  (guix-profile-file name))
        (manifest (guix-manifest-file name)))
    (if (file-exists? manifest)
        (apply system*
               "guix" "package"
               (string-append "--profile=" profile)
               (string-append "--manifest=" manifest)
               rest-args)
        (format (current-error-port)
                "Manifest file '~a' does not exist~%"
                manifest))))

(define (main args)
  (match (cdr args)
    (((or "-h" "--help" "help") _ ...)
     (show-help))
    (((or "-l" "--list" "list") _ ...)
     (display-names (names)))
    (("all" rest-args ...)
     (map (lambda (name)
            (apply populate-profile name rest-args))
          (names)))
    ((name rest-args ...)
     (apply populate-profile name rest-args))
    (_ (show-help))))

;;; profile.scm ends here
