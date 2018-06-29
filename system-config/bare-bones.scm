(use-modules (gnu))

(operating-system
  (host-name "leviafan")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/sda")))
  (initrd (lambda (fs . args)
            (apply base-initrd fs
                   #:extra-modules '("sata_nv") ; for my HDD
                   args)))
  (file-systems (cons (file-system
                        (device (file-system-label "guix"))
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))
  (users (cons (user-account
                (name "al")
                (group "users")
                (supplementary-groups '("wheel" "audio" "video"))
                (home-directory "/home/al"))
               %base-user-accounts))
  (packages %base-packages)
  (services (cons* (console-keymap-service "dvorak")
                   %base-services)))
