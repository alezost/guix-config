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
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages xorg)
  #:use-module (al guix utils))

(define-public my-emacs
  (package (inherit emacs)
    (name "my-emacs")
    (build-system gnu-build-system)
    (inputs (append `(("libxaw" ,libxaw)) ; for the Lucid toolkit
                    (alist-delete "gtk+" (package-inputs emacs))))
    (arguments
     (append `(#:configure-flags '("--with-x-toolkit=lucid"
                                   "--without-gconf"
                                   "--without-gsettings")
               #:make-flags (list ,(cflags)))
             (package-arguments emacs)))
    (synopsis (string-append (package-synopsis emacs)
                             " (with my customizations)"))))

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
               (add-before 'configure 'patch-Makefile
                 ;; Do not build man page (for 'emms-print-metadata')
                 ;; because it is useless and because it makes emms the
                 ;; only emacs package that provides man pages, which
                 ;; leads to (re)building a manual database in my emacs
                 ;; profile after each profile operation.
                 (lambda _
                   (substitute* "Makefile"
                     ;; Remove all lines with 'MAN1DIR'.
                     ((".*MAN1DIR.*") ""))))
               (replace 'pre-install
                 ;; This phase creates "bin" and "man" directories.  I
                 ;; need only "bin".
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out  (assoc-ref outputs "out"))
                          (bin  (string-append out "/bin")))
                     (mkdir-p bin)
                     #t)))
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

                 ;; XXX Campaign header was introduced in Magit 2.11.0
                 ;; and will be removed in the next release:
                 ;;
                 ;; https://github.com/magit/magit/commit/bf71241122e1a0bf707913c87493214ceb12f588
                 ;; https://github.com/magit/magit/commit/4a9d9e59806735100b5d20a8be32defefb659a33
                 ;;
                 ;; Change the value of 'magit-hide-campaign-header'
                 ;; variable since it calls 'git' during initializing.
                 (emacs-substitute-variables "magit-status.el"
                   ("magit-hide-campaign-header" 't))
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
