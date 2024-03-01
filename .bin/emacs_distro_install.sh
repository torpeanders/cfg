#!/usr/bin/env bash

CFGDIR="$HOME/.config/emacs"

check_config_dir () {
  if [ -e $CFGDIR ]; then
        echo "$CFGDIR already exists"
        exit 1
  fi
}

case $1 in
  doom)
    check_config_dir
    echo "Installing Doom Emacs"
    git clone --depth 1 https://github.com/doomemacs/doomemacs $CFGDIR
    $CFGDIR/bin/doom install
    ;;

  spacemacs)
    check_config_dir
    echo "Installing Spacemacs"
    git clone git@github.com:syl20bnr/spacemacs.git $CFGDIR -b develop
    ;;

  *)
    echo "Specify Doom or Spacemacs"
    exit 1
esac
