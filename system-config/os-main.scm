(use-modules
 (srfi srfi-1)
 (gnu)
 (gnu system locale)
 (gnu services networking)
 (gnu services dbus)
 (gnu services desktop)
 (gnu services ssh)
 (gnu services lirc)
 (al places)
 (al files)
 (al utils)
 (al guix packages)
 (al guix services linux)
 (al guix utils))

(define %user-name "al")
(define %group-name "users")
(define %host-name "leviafan")

(define %extra-linux-modules
  '("fuse"              ; for sshfs
    "sata_nv"           ; for my HDD to be recognized
    "snd-seq"           ; for MIDI-keyboard
    ))

(define %redundant-linux-modules
  '("pcspkr" "snd_pcsp"))

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

    (kernel-arguments
     (list (string-append "modprobe.blacklist="
                          (apply comma-separated
                                 %redundant-linux-modules))))

    (initrd (lambda (fs . args)
              (apply base-initrd fs
                     #:extra-modules %extra-linux-modules
                     args)))

    (file-systems
     (cons* (file-system
              (device "guix")
              (title 'label)
              (type "ext4")
              (mount-point "/"))
            (file-system
              (device "storage")
              (title 'label)
              (type "ext4")
              (mount-point "/mnt/storage")
              (create-mount-point? #t)
              (check? #f))
            (file-system
              (device "arch")
              (title 'label)
              (type "ext4")
              (mount-point "/mnt/arch")
              (create-mount-point? #t)
              (check? #f))
            (file-system
              (device "boot")
              (title 'label)
              (type "ext4")
              (mount-point "/mnt/boot")
              (create-mount-point? #t)
              (check? #f))
            (file-system
              (device "teXet")
              (title 'label)
              (type "vfat")
              (mount-point "/mnt/texet")
              (mount? #f)
              (create-mount-point? #t)
              (check? #f)
              (options (comma-separated
                        "rw" "user" "noauto" "utf8" "umask=0002"
                        (string-append "gid=" %group-name))))
            %base-file-systems))

    (users
     (cons* (user-account
             (name %user-name)
             (uid 1000)
             (comment "Alex Kost")
             (home-directory (string-append "/home/" %user-name))
             (group %group-name)
             (supplementary-groups
              '("wheel" "kvm" "audio" "video" "lp" "cdrom")))
            %base-user-accounts))

    (groups
     ;; Use ID 100 for "users" group.  Actually, this wouldn't change ID
     ;; of an existing group, because the following command (called by
     ;; 'add-group' in (gnu build activation) module):
     ;;
     ;;   groupadd -g 100 --system users
     ;;
     ;; fails telling: "group 'users' already exists".
     (replace (lambda (group)
                (string=? "users" (user-group-name group)))
              (user-group (name "users")
                          (id 100)
                          (system? #t))
              %base-groups))


    (sudoers-file (local-file (config-file "etc/sudoers")))
    (hosts-file (local-file (config-file "etc/hosts")))

    (issue "Guix is Great!  Ave Guix!!  Ave!!!\n\n")

    (packages
     (let ((useless-packages (guix-packages
                              (linux iw wireless-tools)
                              (nano nano)
                              (texinfo texinfo)
                              (zile zile))))
       (append (guix-packages
                (admin shadow)
                (certs nss-certs)
                (lirc lirc))
               (my-packages
                (misc suspend))
               xorg-packages
               (remove (lambda (pkg)
                         (memq pkg useless-packages))
                       %base-packages))))

    (services
     (list
      (console-font-service "tty1")
      (console-font-service "tty2")
      (console-font-service "tty3")
      (console-font-service "tty4")
      (console-font-service "tty5")
      (console-font-service "tty6")

      (mingetty-service (mingetty-configuration
                         (tty "tty1")
                         (auto-login %user-name)))
      (mingetty-service (mingetty-configuration
                         (tty "tty2")))
      (mingetty-service (mingetty-configuration
                         (tty "tty3")))
      (mingetty-service (mingetty-configuration
                         (tty "tty4")))
      (mingetty-service (mingetty-configuration
                         (tty "tty5")))
      (mingetty-service (mingetty-configuration
                         (tty "tty6")))

      (login-service (login-configuration
                      (motd (plain-file "motd" "\
Welcome to Hyksos!  I mean GuixOS!  I mean GuixSD!\n\n"))))

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
      (tor-service)
      (static-networking-service "lo" "127.0.0.1"
                                 #:provision '(loopback))

      (udisks-service)
      (polkit-service)
      (elogind-service)
      (dbus-service)
      (lsh-service)
      (syslog-service (syslog-configuration
                       (config-file (local-file
                                     (config-file "syslog/syslog.conf")))))
      (guix-service)
      (nscd-service)
      (udev-service
       #:rules (guix-packages
                (linux lvm2 fuse alsa-utils)))))))

os
