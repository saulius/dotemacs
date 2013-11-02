#!/bin/bash

verify_cask() {
  if hash cask 2>/dev/null; then
    cd ~/.emacs.d
    cask install
  else
    echo "You will need to install Cask (Emacs package manager) and make sure it is in PATH."
    echo "git clone git@github.com:cask/cask.git ~/.cask"
    echo 'export PATH=\$HOME/.cask/bin:\$PATH" >> ~/.bash_profile'
  fi
}

verify_cask
