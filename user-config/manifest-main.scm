;; (use-modules (guix profiles) (gnu))

(use-modules (srfi srfi-1))

(define-syntax-rule (module-packages module package ...)
  (list (@ module package)
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


;;; Packages to build things

(define build-common-packages
  (guix-packages
   (commencement gcc-toolchain-5.1)
   (base gnu-make)
   (autotools autoconf
              automake)
   (pkg-config pkg-config)
   (gettext gnu-gettext)
   (texinfo texinfo)))

(define build-guix-packages
  (append
   build-common-packages
   (guix-packages
    (guile guile-2.0
           guile-json)
    (gnupg libgcrypt)
    (graphviz graphviz)
    (man help2man)
    (databases sqlite))))


;;; GUI

(define xorg-packages
  ;; Xorg server and required modules.
  (guix-packages
   (xorg xorg-server
         xf86-input-evdev
         xf86-video-fbdev
         xf86-video-modesetting
         xf86-video-nv
         xf86-video-nouveau)))

(define xorg-friends-packages
  (guix-packages
   (xorg xrandr
         xmodmap
         setxkbmap
         xrdb
         xset
         xsetroot
         xdpyinfo
         xev
         xinput
         xterm)
   (xdisorg wmctrl
            unclutter
            scrot)
   (openbox openbox)))


(define emacs-packages
  (guix-packages
   (emacs emacs
          emacs-w3m
          emacs-wget)))

(define font-packages
  (guix-packages
   (xorg font-adobe100dpi
         font-adobe75dpi
         font-alias
         font-misc-misc)
   (fonts font-dejavu
          font-gnu-freefont-ttf
          font-liberation
          font-adobe-source-han-sans)
   (fontutils fontconfig)
   (ghostscript gs-fonts)))

(define multimedia-packages
  (append
   (guix-packages
    (pdf zathura
         zathura-pdf-poppler
         zathura-djvu)
    (imagemagick imagemagick)
    (video mplayer
           mpv
           youtube-dl)
    (tv tvtime))
   (my-packages
    (sxiv sxiv-configured))))

(define misc-packages
  (guix-packages
   (linux alsa-utils
          lm-sensors
          sshfs-fuse)
   (file file)
   (glib dbus)
   (gnupg gnupg
          pinentry)
   (gnuzilla icecat)
   (w3m w3m)
   (xml libxslt)
   (wget wget)
   (lirc lirc)
   (ssh openssh)
   (bittorrent rtorrent)

   (version-control git
                    git-manpages)

   (guile guile-2.0)
   (python python-wrapper)

   (aspell aspell
           aspell-dict-en
           aspell-dict-ru)))


(define gui+emacs-packages
  (append xorg-packages
          xorg-friends-packages
          (guix-packages (emacs emacs))))

(define reliable-packages
  ;; Everything reliable without "build".
  (append
   xorg-packages
   xorg-friends-packages
   emacs-packages
   font-packages
   multimedia-packages
   misc-packages))

(define unreliable-packages
  ;; Some terrible people use the same URL for different versions of
  ;; their programs, so when the version changes, the hash of the source
  ;; changes as well, and the package does not work anymore.
  (my-packages
   (fonts font-symbola)))


;;; Manifests

(define-syntax-rule (->manifest packages ...)
  (packages->manifest
   (delete-duplicates (append packages ...) eq?)))

;; (->manifest build-guix-packages)
;; (->manifest gui+emacs-packages)
;; (->manifest reliable-packages)
(->manifest build-guix-packages reliable-packages)
;; (->manifest build-guix-packages reliable-packages unreliable-packages)

;; manifest-main.scm ends here.
