# Jedi
Jedi is an IDE extension which provides support for Python: https://github.com/tkf/emacs-jedi

## Prepare python
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

## set python environment in emacs
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

### installing modules
**Attention:** If you want auto completion for modules you have installed yourself, you need to enable the environment (default: jedi) you use in emacs.

```bash
source $JEDI_ENV_PATH~/.emacs.d/.python-environments/jedi/bin/activate
```

**Attention:** If you have loaded the environment, it is still possible that the `python`, `python3`, `pip` and `pip3` commands point to executables which are not part of the environment. Make sure you run the right tools. To be on safe side, simply run the `load-jedi-python.sh` script.

To verify that the installation was correct, run `M+x run-python RET` in emacs and try to import the module.

# Flycheck

Flycheck is a framework that makes it possible to highlight warnings and errors in source code with external tools. Two external tools are configured for Jedi mode:

* **pycompile:** General Python semantic errors
* **mypy:** static type checking using [type hints](https://mypy.readthedocs.io/en/latest/cheat_sheet_py3.html) [link](http://mypy-lang.org/)

## Install mypy

```bash
pip3 install mypy
```

## Configure Paths to the flycheck tools

```lisp
(custom-set-variables
 ;; placeholder
 '(flycheck-python-flake8-executable "/path/to/python3")
 '(flycheck-python-pycompile-executable "/path/to/python3")
  ;; placeholder
 '(flycheck-python-pylint-executable "/path/to/python3")
 '(flycheck-python-mypy-executable "/path/to/mypy"))
```
