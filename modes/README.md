# Modes

Modes are extensions of Emacs which are used for specific tasks. Modes are independent from the rest of the configuration and can be installed optionally.

# Create a Mode

Each mode contains at least one `.el` file with the same name as the folder. The `.emacs` file uses the `load-file` function to execute its contents. A `README.md` and a `config.json` are optional. The `README.md` file contains information about the mode. The `setup.py` uses the `config.json` to install external tools and set some system dependent values. For the system dependent values the `setup.py` script creates a emacs lisp file called `setup_generated.el`, which is automatically loaded in the init file. The lisp code is direct executed before the `local-vars.el`.

## config.json

The `config.json` has the structure:

``` json
{
  "<mode name 1> : {
	<option> : <value>
	},
  "<mode name 2> : {
	<option 1> : <value>
	<option 2> : <value>
	},
```

Options can be:

* **"comment" : "text"**: Text that is used to explain the configuration of the package.
* **"conda" : ["package 1", "package 2"]**: Install a Conda package in the Emacs Conda environment. You can use all Conda options that are available after the `conda install` command, e.g. `-c conda-forge black`.
* **"lisp" : ["lisp code"]**: The Lisp code is copied directly into the init script. Please note the escaping string.
* **"vars" : {"var name 1" : "value 1", "var name 2" : "[value 21, value 22]" }**: Each dictionary entry of `vars` is transforming into a lisp `(setq ...)` statement. Two types are supported, string and list of string. Depending of the type, it is transformed into `(setq <name> "<value">)` or `(setq <name> '("<value 1" "<value 2">))`. The value type is string each time. If a different type is required, use raw `lisp` code. You can use the `<env_root>` placeholder for paths, e.g. if you want to refer to an executable installed via Conda: `<env_root>/bin/exe`.
