#!/bin/sh

# Author      : Alex Kost <alezost@gmail.com>
# Created     : 07 May 2014
# Description : Make "guix builder" group and users
# License     : GNU GPLv3

#### HELP MESSAGE ##############################################
hlp='Usage: guix-builders
Generate auxiliary group and users for guix daemon.
Should be run as a superuser.
See (info "(guix) Build Environment Setup") for details.'

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    echo "$hlp"
    exit 0
fi
################################################################

# This is taken from the guix info manual.

groupadd --system guixbuild
for i in `seq -w 1 10`;
do
  useradd -g guixbuild -G guixbuild           \
          -d /var/empty -s `which nologin`    \
          -c "Guix build user $i" --system    \
          guixbuilder$i;
done
