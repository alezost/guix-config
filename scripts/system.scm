#!/usr/bin/guile \
-e main -s
!#
;;; system.scm --- Check, build or reconfigure guix system

;; Copyright © 2015 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 13 Jun 2015

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

;; Just a little script to reduce writing for building/reconfiguring my
;; guix system.

;;; Code:

(use-modules
 (ice-9 match)
 (ice-9 regex)
 (srfi srfi-2)
 (al places))

(define %config (config-file "guix/system-config/os-main.scm"))

(define (show-help)
  (display "Usage: system ACTION
Check, build or reconfigure guix system with the current config file.")
  (display "\n
Actions:
  c(heck)
  b(uild)
  r(econfigure)")
  (newline))

(define (action->command action)
  "Return a list with command and its arguments to perform ACTION.
Return #f if ACTION is unknown."
  (define (action? real-action)
    (string-match (string-append "\\`" action)
                  real-action))

  (cond
   ((action? "check")
    (list "guild" "compile"
          "-Wunbound-variable" "-Wunused-variable" "-Wunused-toplevel"
          %config))
   ((action? "build")
    (list "guix" "system" "build" "--no-grub" %config))
   ((action? "reconfigure")
    (list "sudo" "--preserve-env"
          "guix" "system" "reconfigure"
          "--no-grub" "--no-substitutes" %config))
   (else #f)))

(define (main args)
  (unless
      (and-let* ((action (match args
                           ((_ action _ ...)
                            action)
                           (_ #f)))
                 (command (action->command action)))
        (apply system* command))
    (show-help)))

;;; system.scm ends here
