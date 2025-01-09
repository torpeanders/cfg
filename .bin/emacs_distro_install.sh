#!/usr/bin/env bash

set -eu

CHEMACS_DIR="$HOME/.emacs.d"
DOOM_DIR="$HOME/.config/doom"
SPACEMACS_DIR="$HOME/.config/spacemacs"

# install chemacs2
if [ -e $CHEMACS_DIR ]; then
    echo "$CHEMACS_DIR already exists"
    exit 1
else
    echo "Cloning chemacs2"
    git clone https://github.com/plexus/chemacs2 $CHEMACS_DIR >/dev/null 2>&1 
fi

# install spacemacs
if [ -e $SPACEMACS_DIR ]; then
    echo "$SPACEMACS_DIR already exists"
else
    echo "Installing Spacemacs"
    git clone git@github.com:syl20bnr/spacemacs.git $SPACEMACS_DIR -b develop >/dev/null 2>&1 
fi

# install doom emacs
if [ -e $DOOM_DIR ]; then
    echo "$DOOM_DIR already exists"
else
    echo "Installing Doom Emacs"
    git clone --depth 1 https://github.com/doomemacs/doomemacs $DOOM_DIR >/dev/null 2>&1 
    $DOOM_DIR/bin/doom install
fi

