#!/bin/bash

HERE=${PWD}

# Follow user's predetermined method of backups.
export VERSION_CONTROL=existing

function make_link {
    ln -f -s -b "$1" "$2"
}

# Shell / Bash
echo -n "Setting up Bash and shell..."
make_link $HERE/bash/bashrc $HOME/.bashrc
make_link $HERE/bash/dircolors $HOME/.dircolors
echo "done"

# Fonts
echo -n "Setting up fonts..."
mkdir -p $HOME/.fonts
echo -n "deferring font population..."
echo "done"

# Emacs
echo -n "Setting up Emacs..."
emacs_dir=$HOME/.emacs.d
mkdir -p $emacs_dir
make_link $HERE/emacs/init.el $emacs_dir/init.el
make_link $HERE/emacs/jay $emacs_dir/jay
echo "done"

exit 0

