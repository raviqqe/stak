#!/bin/sh

set -e

if [ $(uname) = Linux ]; then
  sudo mkdir -p /usr/lib/x86_64-linux-gnu/libfakeroot
fi

brew install chibi-scheme gambit-scheme gauche guile
cargo install stak
