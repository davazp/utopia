#!/bin/sh
#
# Create symbolic links from my $HOME to this repo
#

EMACS_DIR=~/.emacs.d

mkdir -p $EMACS_DIR

ln -siv $PWD/emacs/init.el $EMACS_DIR
ln -siv $PWD/emacs/lisp/ $EMACS_DIR

mkdir -p $EMACS_DIR/straight/
ln -siv $PWD/emacs/straight/versions $EMACS_DIR/straight
