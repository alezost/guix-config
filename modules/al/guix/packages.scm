;;; packages.scm --- Guix packages I use

;; Copyright © 2015–2019 Alex Kost <alezost@gmail.com>

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
  (specifications->packages
   "autoconf"
   "automake"
   "gcc-toolchain"
   "gettext"
   "libtool"
   "make"
   "pkg-config"
   "texinfo"))


;;; GUI

(define xorg-packages
  ;; Xorg server and required modules.
  (append
   (specifications->packages
    "my-xorg-server"
    "xf86-input-evdev"
    "xf86-input-libinput"
    "xf86-video-fbdev"
    "xf86-video-nouveau")
   (my-packages
    (x xdaemon))))

(define xorg-friends-packages
  (specifications->packages
   "maim"
   "openbox"
   "scrot"
   "setxkbmap"
   "slop"
   "unclutter"
   "wmctrl"
   "xdpyinfo"
   "xdpyprobe"
   "xev"
   "xinput"
   "xlsfonts"
   "xmodmap"
   "xprop"
   "xrandr"
   "xrdb"
   "xset"
   "xsetroot"
   "xterm"))


;;; Other packages

(define emacs-packages
  ;; Emacs packages, but not Emacs itself.
  (specifications->packages
   "emacs-debbugs"
   "emacs-edit-indirect"
   "emacs-elisp-slime-nav"
   "emacs-erc-hl-nicks"
   "emacs-flx"
   "emacs-geiser"
   "emacs-git-modes"
   "emacs-google-translate"
   "emacs-hl-todo"
   "emacs-hydra"
   "emacs-ivy"
   "emacs-markdown-mode"
   "emacs-package-lint"
   "emacs-paredit"
   "emacs-pdf-tools"
   "emacs-rainbow-mode"
   "emacs-shift-number"
   "emacs-smartparens"
   "emacs-smex"
   "emacs-transient"
   "my-emacs-emms"
   "my-emacs-magit"
   "my-emacs-w3m"
   "my-emacs-wget"))

(define guile-packages
  (specifications->packages
   "guile"
   "guile-charting"
   "guile-chickadee"
   "guile-daemon"
   "guile-gcrypt"
   "guile-git"
   "guile-json"
   "guile-sqlite3"
   "guile-xosd"
   "haunt"))

(define font-packages
  (append
   (specifications->packages
    "font-adobe100dpi"
    "font-adobe75dpi"
    "font-misc-misc"
    "font-adobe-source-han-sans"
    "font-dejavu"
    "font-ubuntu"
    "font-gnu-freefont-ttf"
    "font-liberation"
    "gs-fonts")
   (my-packages
    (fonts font-alias-pure
           font-symbola))))

(define multimedia-packages
  (specifications->packages
   "ffmpeg"
   "graphviz"
   "imagemagick"
   "mplayer"
   "mpv"
   "my-sxiv"
   "sox"
   "timidity++"
   "youtube-dl"
   "zathura"
   "zathura-djvu"
   "zathura-pdf-poppler"))

(define misc-packages
  (specifications->packages
   "alsa-utils"
   "aspell"
   "aspell-dict-en"
   "aspell-dict-ru"
   "baobab"
   "curl"
   "dbus"
   "djvulibre"
   "dunst"
   "file"
   "fontconfig"
   "gdb"
   "ghostscript"
   "ghostscript:doc"
   "git"
   "git:send-email"
   "gnupg"
   "gtk-engines"                ; standard themes (clearlooks, etc.)
   "icecat"
   "iotop"
   "libnotify"                  ; for 'notify-send'
   "libxslt"
   "lm-sensors"
   "ltrace"
   "man-db"                     ; to set MANPATH on non-GuixOS
   "man-pages"
   "my-emacs"
   "netcat"
   "openssh"
   "pulseaudio"
   "pavucontrol"
   "pinentry"
   "postgresql"
   "python-wrapper"
   "rsync"
   "rtorrent"
   "sbcl"
   "sshfs"
   "strace"
   "tidy-html"
   "torsocks"
   "unzip"
   "w3m"
   "wget"))

(define unreliable-packages
  ;; Some terrible people use the same URL for different versions of
  ;; their programs, so when the version changes, the hash of the source
  ;; changes as well, and the package does not work anymore.
  (my-packages
   (fonts font-symbola)))

;;; packages.scm ends here
