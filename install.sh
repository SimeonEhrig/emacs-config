#!/bin/bash

# handle directories
project_dir="$(pwd)"
cd $HOME

echo "link ./emacs to $HOME/.emacs"
ln -s $project_dir/emacs .emacs

if [ ! -d "$HOME/.emacs.d" ]; then
    echo "create folder $HOME/.emacs.d"
    mkdir $HOME/.emacs.d
fi

cd $HOME/.emacs.d
echo "link ./lisp folder to $HOME/.emacs.d"
ln -s $project_dir/lisp lisp
