(use-modules
 (al guix packages)
 (al guix utils))

(lists-of-packages->manifest
 xorg-packages
 xorg-friends-packages
 font-packages
 multimedia-packages
 misc-packages
 ;; unreliable-packages
 )
