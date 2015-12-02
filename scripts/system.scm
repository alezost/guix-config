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
 (al places))

(define %config (guix-system-file "main"))

(define (show-help)
  (format #t "Usage: ~a ACTION [ARGS ...]
Check, build or reconfigure Guix system using the following config file:

  ~a"
          (car (command-line)) %config)
  (display "\n
ARGS are the rest arguments that are passed to the according command.

Actions:
  c(heck)               run 'guild compile' with some warning flags;
  b(uild)               run 'guix system build';
  r(econfigure)         run 'guix system reconfigure'.")
  (newline))

(define (action->command action . rest-args)
  "Return a list with command and its arguments to perform ACTION.
Return #f if ACTION is unknown."
  (define (action? real-action)
    (string-match (string-append "\\`" action)
                  real-action))

  (cond
   ((action? "check")
    `("guild" "compile"
      "-Wunbound-variable" "-Wunused-variable" "-Wunused-toplevel"
      ,@rest-args ,%config))
   ((action? "build")
    `("guix" "system" "build" "--no-grub" ,@rest-args ,%config))
   ((action? "reconfigure")
    `("sudo" "--preserve-env"
      "guix" "system" "reconfigure"
      "--no-grub" "--no-substitutes" ,@rest-args ,%config))
   (else #f)))

(define (main args)
  (match (cdr args)
    ((action rest-args ...)
     (let ((cmd (apply action->command action rest-args)))
       (if cmd
           (apply system* cmd)
           (show-help))))
    (_ (show-help))))

;;; system.scm ends here
