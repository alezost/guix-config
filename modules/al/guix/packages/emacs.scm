;;; emacs.scm --- Emacs packages

;; Copyright © 2015, 2017 Alex Kost <alezost@gmail.com>

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

;; Various emacs packages.

;;; Code:

(define-module (al guix packages emacs)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages mp3))

(define-public my-emacs-emms
  (package
    (inherit emms)
    (name "my-emacs-emms")
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

(define-public my-emacs-magit
  (package
    (inherit magit)
    (name "my-emacs-magit")
    (arguments
     (substitute-keyword-arguments (package-arguments magit)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'patch-exec-paths
             (lambda _
               (with-directory-excursion "lisp"
                 ;; "magit.el" calls 'magit-startup-asserts' and
                 ;; 'magit-version' functions in the top level.  I don't
                 ;; need it at all.
                 (emacs-batch-edit-file "magit.el"
                   `(progn (goto-char (point-min))
                           (re-search-forward "(if after-init-time")
                           (up-list -1)
                           (kill-sexp)
                           (basic-save-buffer)))
                 (emacs-substitute-variables "magit-git.el"
                   ("magit-git-executable" "git"))
                 #t)))))))
    (inputs '())
    (synopsis (string-append (package-synopsis magit)
                             " (without git dependency)"))))

(define-public my-emacs-w3m
  (package
    (inherit emacs-w3m)
    (name "my-emacs-w3m")
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
    (synopsis (string-append (package-synopsis emacs-w3m)
                             " (without extra dependencies)"))))

(define-public my-emacs-wget
  (package
    (inherit emacs-wget)
    (name "my-emacs-wget")
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-wget)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'patch-exec-paths)))))
    (inputs '())
    (synopsis (string-append (package-synopsis emacs-wget)
                             " (without wget dependencies)"))))

;;; emacs.scm ends here
