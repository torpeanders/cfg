#!/usr/bin/env bash

set -eu

CHEMACS_DST="$HOME/.emacs.d"
DOOM_DST="$HOME/.config/doom"
SPACEMACS_DST="$HOME/.config/spacemacs"

# install chemacs2
if [ -e $CHEMACS_DST ]; then
    echo "Chemacs is already installed"
else
    echo "Cloning chemacs2"
    git clone https://github.com/plexus/chemacs2 $CHEMACS_DST >/dev/null 2>&1 
fi

# install spacemacs
if [ -e $SPACEMACS_DST ]; then
    echo "Spacemacs is already installed"
else
    echo "Installing Spacemacs"
    git clone git@github.com:syl20bnr/spacemacs.git $SPACEMACS_DST -b develop >/dev/null 2>&1 
fi

# install doom emacs
if [ -e $DOOM_DST ]; then
    echo "Doom is already installed"
else
    echo "Installing Doom Emacs"
    git clone --depth 1 https://github.com/doomemacs/doomemacs $DOOM_DST >/dev/null 2>&1 
    DOOMDIR="$HOME/.doom.d" $DOOM_DST/bin/doom install
fi

