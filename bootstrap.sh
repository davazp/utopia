#!/bin/sh
# 
# Create symbolic links from my $HOME to this repo
# 

EMACS_DIR=~/.emacs.d

mkdir -p $EMACS_DIR

ln -siv $PWD/init.el $EMACS_DIR/init.el
ln -siv $PWD/lisp/ $EMACS_DIR/lisp

mkdir -p $EMACS_DIR/straight/
ln -siv $PWD/straight/versions $EMACS_DIR/straight/versions


