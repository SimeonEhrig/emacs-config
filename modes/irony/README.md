
# company-irony

## irony-install-server with non default libclang.so

If you want to use the irony-server with a clang installation which is not always available, follow these steps. For example, if you are using a module environment system, this is the case. In this case, you have to use an [RPATH](https://en.wikipedia.org/wiki/Rpath) for the linking the executable.

First you need to add the llvm `/lib` folder to the `LD_LIBRARY_PATH` environment variable and the `/bin` folder to the `PATH` environment variable. You will also need to add the llvm CMake configuration file to the search path. After switching to irony mode in emacs, run `M-x irony-install-server`. You will get a line of CMake arguments. Add the following arguments to the existing CMake arguments: 

```
-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON -DCMAKE_INSTALL_RPATH=/path/to/libclang.so
```

## disable the company-irony backend

Sometimes the `company-irony` backend has some problems with the completion. In this case, it might be helpful to use the `company-clang` backend. To use the `company-clang` backend, simply run the emacs function `disable-company-irony`. You can check if it works by running emacs command `company-diag` after executing a completion command.

To disable the `company-irony` backend for an entire project, you can use a [.dir-locals.el](https://www.emacswiki.org/emacs/DirectoryVariables) file. Just add the following line to the file and the backend will disabled.

```lisp
((nil . ((eval . (disable-company-irony)))))
```

# company-clang

## useful variables

Path to the clang executable

```lisp
(setq company-clang-executable "/path/to/clang")
```
