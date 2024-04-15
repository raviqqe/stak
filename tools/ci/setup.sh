#!/bin/sh

set -e

if [ $(uname) = Linux ]; then
  sudo apt install libfakeroot
fi

brew install chibi-scheme gambit-scheme gauche guile
cargo install stak
