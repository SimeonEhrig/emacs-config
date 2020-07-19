# Jedi
Jedi is an IDE extension which provides auto completion support for Python: https://github.com/tkf/emacs-jedi

It is necessary to run the Emacs command `jedi:install-server` one after you have installed Jedi.

## Using different environment

Jedi is installed in the `emacs-tools` environment. By default it supports auto-completion for packages installed in `emacs-tools`. If you want to use auto-completion for packages installed in another environment, you must set the environment variable `VIRTUAL_ENV` (if the Jedi server is already running, you must restart the server). This can be done manually or with the lisp function `conda-env-activate` of Emacs. You can also set the variable `my-python-default-env` which should point to a Python environment. If the environment variable `VIRTUAL_ENV` is not set, it uses the environment of the variable `my-python-default-env` as default source for Jedi.

# Flycheck

Flycheck is a framework that makes it possible to highlight warnings and errors in source code with external tools. Two external tools are configured for Jedi mode:

* **pycompile:** General Python semantic errors, which is provide by the Python interpreter itself.
* **mypy:** Static type checking using [type hints](https://mypy.readthedocs.io/en/latest/cheat_sheet_py3.html) [link](http://mypy-lang.org/)

# Black

[Black](https://github.com/psf/black) is source code formatter. `Black` is not enabled by default for a Python buffer. If you want to use `Black`, you have to enable the `blacken-mode`.
