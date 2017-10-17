;;; packages.scm --- Guix packages I use

;; Copyright © 2015–2017 Alex Kost <alezost@gmail.com>

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

;; Lists of packages that I install in my Guix profiles using
;; "guix package --manifest" facility.

;;; Code:

(define-module (al guix packages)
  #:use-module (al guix utils)
  #:export (build-common-packages
            build-guix-packages
            xorg-packages
            xorg-friends-packages
            emacs-packages
            guile-packages
            font-packages
            multimedia-packages
            misc-packages
            unreliable-packages))


;;; Packages to build things

(define build-common-packages
  (guix-packages
   (commencement gcc-toolchain-7)
   (base gnu-make)
   (autotools autoconf
              automake
              libtool)
   (pkg-config pkg-config)
   (gettext gnu-gettext)
   (texinfo texinfo)))


;;; GUI

(define xorg-packages
  ;; Xorg server and required modules.
  (append
   (guix-packages
    (xorg xorg-server
          xf86-input-evdev
          xf86-input-libinput
          xf86-video-fbdev
          xf86-video-nouveau))
   (my-packages
    (x xdaemon))))

(define xorg-friends-packages
  (append
   (guix-packages
    (xorg setxkbmap
          xdpyinfo
          xev
          xinput
          xlsfonts
          xmodmap
          xprop
          xrandr
          xrdb
          xset
          xsetroot
          xterm)
    (xdisorg wmctrl
             unclutter
             slop
             maim
             scrot
             xdpyprobe)
    (openbox openbox))))


;;; Other packages

(define emacs-packages
  ;; Emacs packages, but not Emacs itself.
  (append
   (guix-packages
    (emacs emacs-debbugs
           emacs-flx
           emacs-hl-todo
           emacs-hydra
           emacs-ivy
           emacs-markdown-mode
           emacs-pdf-tools
           emacs-smartparens
           emacs-smex
           geiser
           git-modes
           paredit))
   (my-packages
    (emacs my-emacs-emms
           my-emacs-magit
           my-emacs-w3m
           my-emacs-wget))))

(define guile-packages
  (guix-packages
   (guile guile-2.2
          guile-git
          guile-daemon
          guile-xosd
          haunt)
   (plotutils guile-charting)))

(define font-packages
  (append
   (guix-packages
    (xorg font-adobe100dpi
          font-adobe75dpi
          ;; font-alias
          font-misc-misc)
    (fonts font-adobe-source-han-sans
           font-dejavu
           font-ubuntu
           font-gnu-freefont-ttf
           font-liberation)
    (ghostscript gs-fonts))
   (my-packages
    (fonts font-alias-pure
           font-symbola))))

(define multimedia-packages
  (append
   (guix-packages
    (pdf zathura
         zathura-pdf-poppler
         zathura-djvu)
    (graphviz graphviz)
    (imagemagick imagemagick)
    (audio sox
           timidity++)
    (video ffmpeg
           mplayer
           mpv
           youtube-dl))
   (my-packages
    (image my-sxiv)
    (misc my-tvtime))))

(define misc-packages
  (append
   (guix-packages
    (linux alsa-utils
           lm-sensors
           ltrace
           sshfs-fuse
           strace)
    (man man-db                 ; to set MANPATH on non-GuixSD
         man-pages)
    (databases postgresql)
    (emacs emacs)
    (file file)
    (rsync rsync)
    (fontutils fontconfig)
    (glib dbus)
    (gnupg gnupg
           pinentry)
    (gnuzilla icecat)
    (w3m w3m)
    (web tidy-html)
    (xml libxslt)
    (curl curl)
    (wget wget)
    (admin netcat)
    (ssh openssh)
    (bittorrent rtorrent)
    (zip unzip)
    (dunst dunst)
    (gtk gtk-engines)           ; standard themes (clearlooks, etc.)
    (gnome baobab
           libnotify)           ; for 'notify-send'

    (version-control git
                     (git "send-email"))

    (gdb gdb)
    (lisp sbcl)
    (python python-wrapper)

    (aspell aspell
            aspell-dict-en
            aspell-dict-ru))
   (my-packages
    (emacs my-emacs))))

(define unreliable-packages
  ;; Some terrible people use the same URL for different versions of
  ;; their programs, so when the version changes, the hash of the source
  ;; changes as well, and the package does not work anymore.
  (my-packages
   (fonts font-symbola)))

;;; packages.scm ends here
