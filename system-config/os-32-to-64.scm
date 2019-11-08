;; This is a simple example of an operating system declaration to move
;; from a 32-bit system to a 64-bit one.  See the thread
;; <http://lists.gnu.org/archive/html/guix-devel/2015-05/msg00392.html>
;; for details.

(use-modules
 (gnu)
 (guix monads)
 (guix store)
 ((guix build utils) #:select (alist-replace))
 (guix packages)
 (gnu packages linux)
 (gnu packages package-management)
 (gnu services base)
 (al guix services linux))

(define linux-libre-x86_64
  (package
    (inherit linux-libre)
    (arguments `(#:system "x86_64-linux"
                 ,@(package-arguments linux-libre)))
    (native-inputs
     (alist-replace
      "kconfig"
      (list (string-append
             (getenv "HOME")
             "/src/guix/gnu/packages/linux-libre-x86_64.conf"))
      (package-native-inputs linux-libre)))))

(define guix-x86_64
  (package
    (inherit guix)
    (arguments `(#:system "x86_64-linux"
                 ,@(package-arguments guix)))))

(define %services
  ;; Make sure guix-service uses 'guix-x86_64' package.
  (map (lambda (service)
         (if (eqv? 'guix (service-type-name (service-kind service)))
             (service guix-service-type
                      (guix-configuration (guix guix-x86_64)))
             service))
       %base-services))

(operating-system
  (host-name "host")
  (timezone "Europe/Moscow")

  (kernel linux-libre-x86_64)

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/sda")))

  (file-systems
   (cons* (file-system
            (device (file-system-label "guix"))
            (mount-point "/")
            (type "ext4"))
          %base-file-systems))

  (users
   (list (user-account
          (name "al")
          (uid 1000)
          (home-directory "/home/al")
          (group "users")
          (supplementary-groups
           '("wheel" "audio" "video")))))

  (packages
   (cons* iproute
          %base-packages))

  (services
   (cons* (service loadkeys-service-type "dvorak")
          %services)))
