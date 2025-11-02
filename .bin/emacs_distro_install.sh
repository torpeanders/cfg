#!/usr/bin/env bash

set -eu

DOOM_DIR="$HOME/.config/emacs"

# install doom emacs
if [ -e $DOOM_DIR ]; then
    echo "$DOOM_DIR already exists"
else
    echo "Installing Doom Emacs"
    git clone --depth 1 https://github.com/doomemacs/doomemacs $DOOM_DIR >/dev/null 2>&1 
    $DOOM_DIR/bin/doom install
fi

