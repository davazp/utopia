#!/bin/sh

mkdir -p ~/.emacs.d/.local/
printenv > ~/.emacs.d/.local/env

echo "Environment file written to ~/.emacs.d/.local/env. Restart emacs."



