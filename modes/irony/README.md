
# company-irony

## irony-install-server with non default libclang.so

If you want to use the irony-server with a clang installation which is not always available, follow these steps. For example, if you are using a module environment system, this is the case. In this case, you have to use an [RPATH](https://en.wikipedia.org/wiki/Rpath) for the linking the executable.

First you need to add the llvm `/lib` folder to the `LD_LIBRARY_PATH` environment variable and the `/bin` folder to the `PATH` environment variable. You will also need to add the llvm CMake configuration file to the search path. After switching to irony mode in emacs, run `M-x irony-install-server`. You will get a line of CMake arguments. Add the following arguments to the existing CMake arguments: 

```
-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON -DCMAKE_INSTALL_RPATH=/path/to/libclang.so
```

If you want to rebuild the server e.g. for testing or updates, you can permanently add cmake arguments using the variable `irony-extra-cmake-args`. Add the following line to your config, to set the cmake arguments:

```lisp
(setq irony-extra-cmake-args '("-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON" "-DCMAKE_INSTALL_RPATH=/path/to/libclang.so"))
```

## disable the company-irony backend

Sometimes the `company-irony` backend has some problems with the completion. In this case, it might be helpful to use the `company-clang` backend. To use the `company-clang` backend, simply run the emacs function `disable-company-irony`. You can check if it works by running emacs command `company-diag` after executing a completion command.

To disable the `company-irony` backend for an entire project, you can use a [.dir-locals.el](https://www.emacswiki.org/emacs/DirectoryVariables) file. Just add the following line to the file and the backend will disabled.

```lisp
((nil . ((eval . (disable-company-irony)))))
```

## Setup your project

It is really difficult to set up your project the first time because the documentation is not good and some diagnostic features are missing. The following steps describe what you need to do to set up your project and what you can do to solve problems with your project.

**Hint:** Small projects that are simply compiled via `clang src.cpp -o exe` work really well, since auto-completion uses the same mechanism as clang to find system headers. If you are using more complex projects that use a build system, the mechanism is disabled. And that also caused some problems with autocompletion.

### 1. Get a compile_commands.json

The compile\_commands.json (short cdb for compile database) contains the compiler flags for each source file. There are several ways to generate the cdb that are described [here](https://sarcasm.github.io/notes/dev/compilation-database.html).

For example, if you have a CMake Project, simply add the cmake argument `-DCMAKE_EXPORT_COMPILE_COMMANDS=ON`.

**Hint:** To get the best autocompletion, use the Clang as compiler for your project. This is because Irony uses libClang for autocompletion. This means that the Irony autocompletion server takes the same steps as the Clang compiler to analyze the source code and obtain the AST. So if Clang can compile the project, autocompletion will work as well as possible. If you are using GCC, autocompletion should also work, since Clang is compatible with many GCC arguments, but some arguments may not be supported.

### 2. Select the right compilation database

Irony tries to automatically find the correct cdb file. To check if the correct cdb is used, you can run the emacs command `irony-cdb-menu`. A second way to verify that the correct argument has been passed is to vie the Irony server's log. It is stored in the `/tmp` file system. Just open it and compare it with the compile\_commands.json file of your project.

If the cdb was not found, you can set manually the path with the emacs function `irony-cdb-json-add-compile-commands-path`. **Attention:** The first path describes the root path of your source code files. The second path points to the compile\_commands.json. For example, you have the following project structure:

```
|
|-build
|-project
  |-include
    |-foo.h
  |-src
    |-main.cpp
    |-foo.cpp
```

The first path should be set to `project/` to apply the cdb file to `foo.h`, `foo.cpp` and `main.cpp`. The second path should set to `build/compile_commands.json`.

`irony-cdb-json-select` allows you to select different cdb files for a source code root. This can be useful if you use different preprocessor definitions for different builds. Irony can resolve them and vary the completion results. This can happen, for example, if a `#ifdef` is used.

### 3. Fix false results

Sometimes it is not enough to choose the right cdb to get a correct autocompletion. Unfortunately, the cdb file often does not contain all the necessary information to compile a source file correctly. To check what goes wrong with the completion, run the emacs function `irony-print-diagnostic`[1] after the completion. An error message will be printed into the `Message` buffer, which contains the same errors as the clang if it failed to compile.

A common error is that the system headers for the C++ std library are not found. This happens because the automatic search of the system headers is disabled by the complex commands of the cdb. To add the path of the system headers, simply write the following code into your emacs configuration:

```lisp
(setq irony-additional-clang-options
      (append '("-I" "/path/to/[sys]headers") irony-additional-clang-options))
```

Default paths of the headers are:

* **system GCC**: /usr/include/x86_64-linux-gnu/c++/7
* **llvm** : llvm/include/c++/v1/

[1] `irony-print-diagnostic` is not part of the Irony package. It is defined in the `irony.el` of this folder.

# company-clang

## useful variables

Path to the clang executable

```lisp
(setq company-clang-executable "/path/to/clang")
```
