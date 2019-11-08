(use-modules
 (srfi srfi-1)
 (gnu)
 (gnu system locale)
 (gnu services networking)
 (gnu services dbus)
 (gnu services desktop)
 (gnu services ssh)
 (gnu packages base)            ; for 'canonical-package'
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
  '("fuse"                      ; for sshfs
    "nbd"                       ; to mount qcow2 images
    "sata_nv"                   ; for my HDD to be recognized
    "snd-seq"                   ; for MIDI-keyboard
    ))

(define %redundant-linux-modules
  '("pcspkr" "snd_pcsp"))

(define %redundant-packages
  '("info-reader"
    "iw"
    "nano"
    "net-tools"
    "wireless-tools"
    "zile"))

(define os
  (operating-system
    ;; (locale-libcs
    ;;  (cons (guix-package base glibc-2.23)
    ;;        %default-locale-libcs))

    (host-name %host-name)
    (timezone "Europe/Moscow")

    (locale "en_US.utf8")
    (locale-definitions
     (list (locale-definition (source "en_US")
                              (name   "en_US.utf8"))
           (locale-definition (source "ru_RU")
                              (name   "ru_RU.utf8"))))

    (bootloader
     ;; Since I always use "guix system build --no-bootloader", I don't want
     ;; to build grub, but guix wants to build it anyway (it is done by
     ;; 'perform-action' procedure in (guix scripts system) module).  So
     ;; I simply replace the default 'grub' with my 'empty-package'.
     (bootloader-configuration
      (bootloader (bootloader
                   (inherit grub-bootloader)
                   (name 'fake-grub)
                   (package (my-package misc empty-package))))
      (target "/dev/sda")
      (theme (grub-theme))))

    (kernel-arguments
     (list (string-append "modprobe.blacklist="
                          (apply comma-separated
                                 %redundant-linux-modules))))

    (initrd-modules (append %extra-linux-modules %base-initrd-modules))

    (file-systems
     (cons* (file-system
              (device (file-system-label "guix"))
              (type "ext4")
              (mount-point "/"))
            (file-system
              (device (file-system-label "storage"))
              (type "ext4")
              (mount-point "/mnt/storage")
              (create-mount-point? #t)
              (check? #f))
            (file-system
              (device (file-system-label "arch"))
              (type "ext4")
              (mount-point "/mnt/arch")
              (create-mount-point? #t)
              (check? #f))
            (file-system
              (device (file-system-label "boot"))
              (type "ext4")
              (mount-point "/mnt/boot")
              (create-mount-point? #t)
              (check? #f))
            (file-system
              (device "/dev/sr0")
              (type "iso9660")
              (mount-point "/mnt/cdrom")
              (mount? #f)
              (create-mount-point? #t)
              (check? #f)
              (options (comma-separated "ro" "user" "noauto")))
            (file-system
              (device (file-system-label "teXet"))
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
              ;; "input" and "tty" are needed to start X server without
              ;; root permissions: "input" - to access "/dev/input"
              ;; devices, "tty" - to access "/dev/ttyN".
              '("wheel" "kvm" "audio" "video" "input" "tty" "lp" "cdrom")))
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
     (append (specifications->packages
              "nss-certs" "iptables")
             (my-packages
              (misc suspend))
             xorg-packages
             (remove-packages %redundant-packages
                              %base-packages)))

    (services
     (list
      (service virtual-terminal-service-type)
      (service console-font-service-type
               (map (lambda (tty)
                      (cons tty %default-console-font))
                    '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))

      (service agetty-service-type
               (agetty-configuration
                (extra-options '("-L"))
                (term "vt100")
                (tty #f)))

      (service mingetty-service-type
               (mingetty-configuration (tty "tty1")
                                       (auto-login %user-name)))
      (service mingetty-service-type
               (mingetty-configuration (tty "tty2")))
      (service mingetty-service-type
               (mingetty-configuration (tty "tty3")))
      (service mingetty-service-type
               (mingetty-configuration (tty "tty4")))
      (service mingetty-service-type
               (mingetty-configuration (tty "tty5")))
      (service mingetty-service-type
               (mingetty-configuration (tty "tty6")))

      (service login-service-type
               (login-configuration
                (motd (plain-file "motd" "\
Welcome to Hyksos!  I mean GuixOS!\n\n"))))

      (console-keymap-service (local-file
                               (config-file "kbd/dvorak-alt.map")))
      (keycodes-from-file-service (local-file
                                   (config-file "kbd/scancodes-msmult")))

      (service tor-service-type)
      (service dhcp-client-service-type)
      (service static-networking-service-type
               (list ;; (static-networking (interface "enp0s7")
                     ;;                    (ip "192.168.1.32")
                     ;;                    (gateway "192.168.1.1")
                     ;;                    (name-servers '("77.88.8.8")))
                     (static-networking (interface "lo")
                                        (ip "127.0.0.1")
                                        (provision '(loopback)))))

      (udisks-service)
      (service polkit-service-type)
      (service elogind-service-type
               (elogind-configuration
                (handle-suspend-key 'ignore)))
      (dbus-service)
      (service openssh-service-type (openssh-configuration))
      (syslog-service (syslog-configuration
                       (config-file (local-file
                                     (config-file "syslog/syslog.conf")))))
      (service urandom-seed-service-type)
      (service guix-service-type)
      (service nscd-service-type)
      (service udev-service-type
               (udev-configuration
                (rules (specifications->packages
                        "alsa-utils" "fuse"))))
      (service special-files-service-type
               ;; Using 'canonical-package' as bash and coreutils
               ;; canonical packages are already a part of
               ;; '%base-packages'.
               `(("/bin/sh"
                  ,(file-append (canonical-package
                                 (guix-package bash bash))
                                "/bin/bash"))
                 ("/bin/bash"
                  ,(file-append (canonical-package
                                 (guix-package bash bash))
                                "/bin/bash"))
                 ("/usr/bin/env"
                  ,(file-append (canonical-package
                                 (guix-package base coreutils))
                                "/bin/env"))))))))

os
