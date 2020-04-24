# emacs-config
My personal emacs configuration

# Get current emacs version
## via apt
**Source:** https://launchpad.net/%7Ekelleyk/+archive/ubuntu/emacs

```bash
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get update
sudo update-alternatives --config emacs
```

## via spack

```bash
spack install emacs +X +tls toolkit=athena
```

Without gnutools, the libxml2 is missing and the dict.cc addon is not working.

# Content of the Repository

The following section describes the functions of the files and folders.

## lisp

The folder contains third-party packages that are not available in the package manager or are modified versions of the package versions. It also contains self-written functions. All functions in the `autoload.el` file are loaded at start up.

## load-jedi-python.sh

The environment of the jedi python installation. See `modes/jedi`.

## modes

Contains various packages or compilations of packages that are not part of the standard installation. Often the modes require additional installation steps. To read the necessary steps, go to the `modes/*` folder and read the `README.md`. The `setup.py` asks if you want to install a mode.

## setup.py

The `setup.py` script installs and updates the emacs configuration. It creates different folders and creates different links. It is an interactive script that asks which mode to install. The script create the file `modes.json`. It saves your decisions about the modes.

## dir-locals-example

Examples for `.dir-locals.el` for different workflows.

# Where can I find what

* **keybindings:** In the .emcas file in the section *global key bindings* or in the *.el files in the `modes/*` folder.
* **Installed packages via package manager:** In the .emcas file in the section *install packages via package manager* or in the *.el files in the `modes/*` folder.
* **necessary system packages:** The package names are stored in the `setup.py` in the global variable `apt_progs`.
* **which modes are installed:** Stored in the `modes.json`.
* **system-dependent configuration:** Some configurations are system dependent, e.g. paths to executables like clang or cmake. To specify system-dependent configurations, simply create a `local-vars.el` in your `.emacs.d` directory and write your emacs lisp code.

# Links to Projects

* flyspell-popup
  * flyspell-popup is an extension of flyspell that displays the suggestions in a popup instead of a mini buffer on the top
  * https://github.com/xuchunyang/flyspell-popup
* company
  * company is a universal auto complete system. It can be used for different languages like, plain text, C++, Python and so on
  * https://company-mode.github.io/

# Useful system-dependent variables

```list
;; set paths on which projectile search for projects
(setq projectile-project-search-path '("~/projects/" "~/work/"))
```

# Macros
Steps to describe creating a macro, saving and loading it

## create macros

Enter `<f3>` to start recording, then the sequence and save it with `<f4>`. You can edit the macro with `C-x e`. The `name-last-kbd-macro` (`C-x C-k n`) command names the last macro. Now it can be called via `M-x`. With `C-x C-k b` you can temporarily bind the macro to a key.

## save a macro
Open a file, for example `~/.emacs.d/macros/gen.macs`. Go to the end and copy the last macro with `insert-kbd-macro`. Save it finally.

## load the macro
You can simply load the macros from file with `load-file`. If you want to load a macro at start up, just add a line to the `.emacs` file. You can also use the `global-set-key` function to bind the macro to a key combination at startup.

# Emacs Server
The Emacs server makes it possible to run emacs independently of your login season. It is also possible to open a buffer with many (different) clients. To avoid an error that terminates the server when an ssh connection with X forwarding is closed, install emacs using the x toolkit `athena` or `lucid`.

The following commands allow you to start and connect to an emacs server:

``` bash
export EMACS_SERVER_NAME=server-name
# start emacs server
# to stop the server, run server-shutdown inside an emacs-client
alias emacs-server-start="emacs --bg-daemon=$EMACS_SERVER_NAME $@"
# emacs terminal client
alias emc="emacsclient -nw -s $EMACS_SERVER_NAME -c $@"
# emacs GUI client (X11)
alias emgc="emacsclient -s $EMACS_SERVER_NAME -c $@ &"
```

With the command `M-x server-shutdown` you can stop the emacs server from an emacs client.

**Hint:** You can connect an emacs server with terminal and GUI client (simultaneously). It is very helpful if you work from different locations on a remote system.
