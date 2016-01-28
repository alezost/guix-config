(use-modules
 (gnu)
 (gnu system locale)
 (gnu services networking)
 (gnu services dbus)
 (gnu services desktop)
 (gnu services ssh)
 (gnu services lirc)
 (al places)
 (al files)
 (al guix services linux)
 (al guix utils))

(define %user-name "al")
(define %host-name "leviafan")

(define %linux-modules
  '("fuse"              ; for sshfs
    "sata_nv"           ; for my HDD to be recognized
    "snd-seq"           ; for MIDI-keyboard
    ))

(define os
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

    (initrd (lambda (fs . args)
              (apply base-initrd fs
                     #:extra-modules %linux-modules
                     args)))

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
              (type "ext4"))
            %base-file-systems))

    (swap-devices '("/dev/disk/by-label/swap"))

    (users
     (cons* (user-account
             (name %user-name)
             (uid 1000)
             (comment "Alex Kost")
             (home-directory (string-append "/home/" %user-name))
             (group "users")
             (supplementary-groups
              '("wheel" "audio" "video" "lp" "cdrom")))
            %base-user-accounts))

    (sudoers-file (local-file (config-file "etc/sudoers")))
    (hosts-file (local-file (config-file "etc/hosts")))

    (issue "Guix is Great!  Ave Guix!!  Ave!!!\n\n")

    (packages
     (append (guix-packages
              (linux iproute)
              (certs nss-certs))
             %base-packages))

    (services
     (let ((motd (plain-file "motd" "\
Welcome to Hyksos!  I mean GuixOS!  I mean GuixSD!\n\n")))
       (list
        (console-font-service "tty1")
        (console-font-service "tty2")
        (console-font-service "tty3")
        (console-font-service "tty4")
        (console-font-service "tty5")
        (console-font-service "tty6")

        (mingetty-service (mingetty-configuration
                           (tty "tty1") (motd motd)
                           (auto-login %user-name)))
        (mingetty-service (mingetty-configuration
                           (tty "tty2") (motd motd)))
        (mingetty-service (mingetty-configuration
                           (tty "tty3") (motd motd)))
        (mingetty-service (mingetty-configuration
                           (tty "tty4") (motd motd)))
        (mingetty-service (mingetty-configuration
                           (tty "tty5") (motd motd)))
        (mingetty-service (mingetty-configuration
                           (tty "tty6") (motd motd)))

        (rmmod-service "pcspkr")
        (console-keymap-service (local-file
                                 (config-file "kbd/dvorak-alt.map")))
        (keycodes-from-file-service (local-file
                                     (config-file "kbd/scancodes-msmult")))
        (lirc-service #:device "name=i2c*" #:driver "devinput"
                      #:config-file (local-file
                                     (config-file "lirc/devinput.conf")))

        (dhcp-client-service)
        ;; (static-networking-service "enp0s7" "192.168.1.32"
        ;;                            #:gateway "192.168.1.1"
        ;;                            #:name-servers '("77.88.8.8"))
        (static-networking-service "lo" "127.0.0.1"
                                   #:provision '(loopback))

        (udisks-service)
        (polkit-service)
        (elogind-service)
        (dbus-service)
        (lsh-service)
        (syslog-service #:config-file (local-file
                                       (config-file "syslog/syslog.conf")))
        (guix-service)
        (nscd-service)
        (udev-service
         #:rules (guix-packages
                  (linux lvm2 fuse alsa-utils))))))))

os
