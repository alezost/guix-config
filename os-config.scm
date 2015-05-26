(use-modules
 (gnu)
 (guix store)
 (gnu system locale)
 (gnu system grub)
 (gnu packages linux)
 (gnu services base)
 (gnu services networking)
 (gnu services dbus)
 (gnu services ssh)
 (gnu services lirc)
 (al places)
 (al files)
 (al guix services linux))

(define %user-name "al")
(define %host-name "leviafan")

(operating-system
  (host-name %host-name)
  (timezone "Europe/Moscow")

  (locale "en_US.utf8")
  (locale-definitions
   (list (locale-definition (source "en_US")
                            (name   "en_US.utf8"))
         (locale-definition (source "ru_RU")
                            (name   "ru_RU.utf8"))))

  (bootloader
   (grub-configuration (device "/dev/sda")
                       (theme (grub-theme))))

  (file-systems
   (cons* (file-system
            (device "guix")
            (title 'label)
            (mount-point "/")
            (type "ext4"))
          (file-system
            (device "storage")
            (title 'label)
            (mount-point "/mnt/storage")
            (create-mount-point? #t)
            (type "ext4"))
          (file-system
            (device "arch")
            (title 'label)
            (mount-point "/mnt/arch")
            (create-mount-point? #t)
            (type "ext4"))
          (file-system
            (device "boot")
            (title 'label)
            (mount-point "/mnt/boot")
            (create-mount-point? #t)
            (type "ext2"))
          %base-file-systems))

  (swap-devices '("/dev/disk/by-label/swap"))

  (users
   (list (user-account
          (name %user-name)
          (uid 1000)
          (comment "Alex Kost")
          (home-directory (string-append "/home/" %user-name))
          (group "users")
          (supplementary-groups
           '("wheel" "audio" "video" "lp")))))

  (sudoers (read-file (config-file "sudo/sudoers")))

  (packages
   (cons*
    iproute
    %base-packages))

  (services
   (let ((motd (text-file "motd" "Welcome to Hyksos!  I mean GuixOS!  I mean GuixSD!\n\n")))
     (list
      (lirc-service #:device "name=i2c*" #:driver "devinput"
                    #:config-file (config-file "lirc/lirc.conf"))
      (rmmod-service "pcspkr")
      (console-keymap-service (config-file "kbd/dvorak-alt.map"))
      (keycodes-from-file-service (config-file "kbd/scancodes-msmult"))

      (console-font-service "tty1")
      (console-font-service "tty2")
      (console-font-service "tty3")
      (console-font-service "tty4")
      (console-font-service "tty5")
      (console-font-service "tty6")

      (mingetty-service "tty1" #:motd motd #:auto-login %user-name)
      (mingetty-service "tty2" #:motd motd)
      (mingetty-service "tty3" #:motd motd)
      (mingetty-service "tty4" #:motd motd)
      (mingetty-service "tty5" #:motd motd)
      (mingetty-service "tty6" #:motd motd)

      ;; (dhcp-client-service)
      (static-networking-service "enp0s11" "192.168.1.32"
                                 #:gateway "192.168.1.1"
                                 #:name-servers '("77.88.8.8"))
      (static-networking-service "lo" "127.0.0.1"
                                 #:provision '(loopback))

      (dbus-service '())
      (lsh-service)
      (syslog-service #:config-file (config-file "syslog/syslog.conf"))
      (guix-service)
      (nscd-service)
      (udev-service #:rules (list lvm2 fuse alsa-utils))))))
