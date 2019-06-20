#!/bin/bash

# This scripts enable the python environment with the name jedi set aliases for python, python3, pip and pip3

JEDI_ENV_PATH=~/.emacs.d/.python-environments/jedi/

source $JEDI_ENV_PATH/bin/activate

alias python=$JEDI_ENV_PATH/bin/python
alias python3=$JEDI_ENV_PATH/bin/python3
alias pip=$JEDI_ENV_PATH/bin/pip
alias pip3=$JEDI_ENV_PATH/bin/pip3
