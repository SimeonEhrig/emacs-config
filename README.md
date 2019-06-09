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
