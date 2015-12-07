#!/bin/sh

# Author      : Alex Kost <alezost@gmail.com>
# Created     : 19 Mar 2015
# Description : Make /usr/bin a link pointing to a Guix user profile
# License     : GNU GPLv3

# Commentary:

# Problem: Guix system does not have "/usr" directory, but we still want
# to run our python, guile and other scripts that rely on
# "#!/usr/bin/…".  What to do?  I just install all I need in my guix
# profile and make "/usr/bin" a symlink to it.  Problem solved.

# Note: run it like this: "sudo -E …/guix-usr-bin.sh".

# Code:

[[ -d "/usr" ]] || mkdir -v "/usr"

if [[ -a "/usr/bin" ]]; then
    echo "/usr/bin already exists"
    exit 1
else
    # ln -sv "$HOME/.guix-profile/bin" "/usr/bin"
    ln -sv "$HOME/.guix-profiles/main/main/bin" "/usr/bin"
fi
