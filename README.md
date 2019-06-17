# emacs-config
My personal emacs configuration

# Installation
- Run `install.sh` or link/copy emacs to $HOME/.emacs and lisp to $HOME/.emacs.d/
- If you did not run the `install.sh`, create the folder `$HOME/.emacs.d/backup`

# get current version
**Source:** https://launchpad.net/%7Ekelleyk/+archive/ubuntu/emacs

```bash
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get update
sudo update-alternatives --config emacs
```

# Installation of extra packages
**RET** in emacs means the enter key

## Melpa
Melpa is a package source for extra packages

```
# run in emacs
M-x package-refresh-contents RET
```

## xclip
xclip and allow copies between emacs and X11

```bash
apt install xclip
```

```
# run in emacs
M-x package-install RET xclip RET
```

## markdown mode
```
# run in emacs
M-x package-install RET markdown-mode RET
```

## flyspell-popup
flyspell-popup is an extension of flyspell that displays the suggestions in a popup instead of a mini buffer on the top: https://github.com/xuchunyang/flyspell-popup
```
# run in emacs
M-x package-install RET flyspell-popup RET
```

## company
company is a universal auto complete system. It can be used for different languages like, plain text, C++, Python and so on: https://company-mode.github.io/
```
# run in emacs
M-x package-install RET company RET
```

# IDEs
IDEs are really complex systems. Sometimes it takes some work to install an IDE. Therefore, it is better to disable this extension when it is not in use. The IDE extensions are located in an additional section in the point file.  So they can easily find and comment out.

## Jedi
Jedi is an IDE extension which provides support for Python: https://github.com/tkf/emacs-jedi

### Prepare python
Often the system python is not used due of compatibility and user permissions. This tutorial describes how to use a (mini)conda python with emacs-jedi.

Install (mini)conda and pip and virtualenv with conda.

```bash
# install conda
wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh
chmod u+x Miniconda3-latest-Linux-x86_64.sh
./Miniconda3-latest-Linux-x86_64.sh

# maybe you need to add conda to $PATH by yourself
conda install pip virtualenv
```

### install jedi in emacs
```
# run in emacs
M-x package-install RET epc RET
```

```
# run in emacs
M-x package-install RET auto-complete RET
```

```
# run in emacs
M-x package-install RET jedi RET
```

### install company-jedi
```
# run in emacs
M-x package-install RET company-jedi RET
```

### set python environment in emacs
Create a virtual environment and setup in emacs. In this example, the environment has the name "jedi".
https://archive.zhimingwang.org/blog/2015-04-26-using-python-3-with-emacs-jedi.html

```bash
# Attention: Make sure that you use the conda tools and not the system tools.
mkdir -p ~/.emacs.d/.python-environments
virtualenv -p /<path_to_conda>/bin/python3 ~/.emacs.d/.python-environments/jedi
# If you feel like installing the server with 'M-x jedi:install-server', also do the following
~/.emacs.d/.python-environments/jedi/bin/pip install --upgrade ~/.emacs.d/elpa/jedi-20150109.2230/  # you might need to change the version number
```

If you use a name other than "jedi" for your environment, you must also change the line `(setq jedi:environment-root "jedi")` in the dot file.
