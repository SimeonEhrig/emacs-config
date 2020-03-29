# emacs-sphinx-build
Emacs mode which allows to easily create Sphinx documentation and view it in a web browser.

# Install

```bash
;; add to init file
(load-file /path/to/sphinx-build.el)
(add-hook 'rst-mode-hook 'sphinx-build-mode)
```

# Usage

The minor made has to main functions `sphinx-build` and `sphinx-build-and-refresh`.

* Run `sphinx-build` to build Sphinx documentation (`make html`). If the variable `sphinx-makefile-path` is empty, a dialog for setting is started. The path is valid during the session. To set it permanent, see next section.
* Run `sphinx-build-and-refresh` to build the documentation (see `sphinx-build`) and refresh web browser instance. The function just refresh the current page in a open web browser. The function does not open a web browser or change the tab.

# Supported Web Browser

* Mozilla Firefox
* Chromium

# Make `sphinx-makefile-path` persistent

Create a `dir-locals.el` file and add:

``` lisp
((nil .
      ((sphinx-makefile-path . "/path/to/sphinx/docs/makefile"))
      ))
```
