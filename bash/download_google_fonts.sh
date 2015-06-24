#!/bin/bash
################################################
# Nathan Bohman
#   rev. 2 Jay Windley
# 2013-05-02
# Based on manual instructions
# https://code.google.com/p/googlefontdirectory/
#################################################

gfdir=~/.googlefontdirectory   # temporary for Google fonts
fdir=~/.fonts                  # X server convention for user font library

# Set up directories if they do not exist and clone the Google font
# directory locally.
#
mkdir -p $fdir
if [ ! -d $gfdir ]; then       # bootstrap Mercurial
    mkdir -p $gfdir
    hg clone https://googlefontdirectory.googlecode.com/hg/ $gfdir > /dev/null
fi

cd "$gfdir"
hg pull $gfdir > /dev/null     # Mercurial synch

# Copy into per-user font directory.
find . -iname "*.otf" -o -iname "*.ttf" -exec cp {} $fdir \;
