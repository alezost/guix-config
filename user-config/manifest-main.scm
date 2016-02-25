(use-modules
 (al guix packages)
 (al guix utils))

(lists-of-packages->manifest
 xorg-friends-packages
 multimedia-packages
 misc-packages
 ;; unreliable-packages
 )
