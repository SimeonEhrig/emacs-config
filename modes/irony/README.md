
# Company-irony

## Irony-install-server with non default libclang.so

If you want to use the irony-server with a clang installation which is not always available, follow these steps. For example, if you are using a module environment system, this is the case. In this case, you have to use an [RPATH](https://en.wikipedia.org/wiki/Rpath) for the linking the executable.

First you need to add the llvm `/lib` folder to the `LD_LIBRARY_PATH` environment variable and the `/bin` folder to the `PATH` environment variable. You will also need to add the llvm CMake configuration file to the search path. After switching to irony mode in emacs, run `M-x irony-install-server`. You will get a line of CMake arguments. Add the following arguments to the existing CMake arguments:

```
-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON -DCMAKE_INSTALL_RPATH=/path/to/libclang.so
```

If you want to rebuild the server e.g. for testing or updates, you can permanently add cmake arguments using the variable `irony-extra-cmake-args`. Add the following line to your config, to set the cmake arguments:

```lisp
(setq irony-extra-cmake-args '("-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON" "-DCMAKE_INSTALL_RPATH=/path/to/libclang.so"))
```

## Disable the company-irony backend

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

## company-irony-c-headers

Company-irony-c-headers allows auto completion for header includes.

```lisp
(setq company-irony-c-headers--compiler-executable "/path/to/clang++")
```

View the header with `C-w` in another buffer when opening the completion suggestions.

# Company-clang

## Useful variables

Path to the clang executable

```lisp
(setq company-clang-executable "/path/to/clang")
```

# Rtags

[Rtags](https://github.com/Andersbakken/rtags) is a client-server implementation of the Clang toolkit. The server (rdm) uses the Clang toolkit to parse C++ files and generate an AST and index. The client (rc) is responsible for obtaining various information about the parsed project. Rtags is designed as an editor independent tool. So the server (rdm) and the client (rc) are standalone applications and the emacs functions call the client application with different parameters.

## Install

Since the server and client are standalone applications, it is necessary to compile them after installing the rtags emacs package.

```lisp
;; run the following command in scratch after you have activated the rtags mode
;; -DLIBCLANG_LLVM_CONFIG_EXECUTABLE is only necessary if you have not installed the clang toolchain in the default path
(rtags-install nil "-DRTAGS_NO_ELISP_FILES=1 -DLIBCLANG_LLVM_CONFIG_EXECUTABLE=/path/to/llvm-config")
```

After compiling and installing, it make sense to add the `bin` folder path to your `PATH` variable, as there is no emacs function to initialize a project index. The usual path is `.emacs.d/elpa/rtags-<hash>/rtags-<version>/bin`.

### General hints

With `rc --help` you get all options to control the server. The `rc -g` command prints the server log to the terminal.

## Setup project

The most difficult part is to set up a project initialization. When a project is running, rtags takes care of updates automatically.

There are two requirements for using rtags in a project (read all points first to avoid additional work):
1. You need a build of the project
    - Clang must be able to compile the project
    - Often the build of the project can be done with `gcc` because many arguments are compatible
2. The build needs a `compile_commands.json`.
    -  How to get it is explained in [`1. Get a compile_commands.json`](https://github.com/SimeonEhrig/emacs-config/tree/master/modes/irony#1-get-a-compile_commandsjson) section of the irony section in this README file.

When the build is complete, you must index a project. Indexing is done using the Bash tool. Make sure that the server is running, otherwise you will get an error message. There are two ways to start the server. The first is directly through the bash command `rdm`. The second is via the emacs function `rtags-start-process-unless-running`. How they start the server doesn't matter. Each time the server is running, the same data is used and it is available on the same socket. So you can also mix the startup methods and it will still work in emacs.

To index a project, run the command:

```bash
# initialize project
rc -J /path/to/compile_commands.json
```

The process runs asynchronous and take a little bit time depending on the project. If your project is very large, it make sense to display the server log via `rc -g` on a second terminal. Indexing is complete when there is a message like `Jobs took 0.23s. We're using 40mb of memory.` is displayed.

To check the success of indexing, run `rtags-find-symbol-at-point` (M-.) on a word in your source code. Especially function definition and implementation in different files (`.h` and `.cpp`) are a really good test case. If you switch between the files, everything is fine, otherwise see next section.

### Solving indexing problems

To solve the indexing problem, do the following workflow:

1. Open an affected source code file
2. Run the command `rtags-diagnostics` to open the diagnostic buffer. The buffer displays all clang compiler errors.
3. Run the command `rtags-reparse-file` to reparse the file.
4. Read the clang errors in the diagnostic buffer.
5. Solve the problem [1]

[1] This point is work in progress and will be extended as new problems are found and solved. See next section.

#### Solutions for various problems

**Missing headers:** If header files are missing, e.g. the std lib, you can add an include path when you start rdm. Depending on how do you start the server, you may need to use the following code snippets.

```bash
rdm -I/path/to/header -I/another/path
```

```lisp
(setq rtags-rdm-includes '("/path/to/header -I/another/path"))
```

Then, reparse the file.


**Sets start arguments for rdm:**
```lisp
(setq rtags-process-flags "-I/path/to/header")
```

# Flycheck

The C++ mode uses two backends for flycheck: `irony flychek` and `rtags flycheck`.

- **irony flychek**: Is enabled by default. Will automatically disable when a project is detected (e.g. `.git` or `CMakeLists.txt`), because the `irony flycheck` crashes slightly if it is not configured correctly. To reactivate it, configure `irony` and set the variable `my/cpp-force-irony-flycheck` to `t` in the project file.
- **rtags flycheck**: Is disabled by default. Configure `rtags` and set variable `my/cpp-force-rtags-flycheck` to `t` in the project file.

# cmake-ide

`cmake-ide` is a utility package which reads cmake builds and set up paths for `irony` and `rtags`. If `cmake-ide` does not detect the build folder correctly, set up the variable `cmake-ide-dir`. Then `cmake-ide` runs automatically.
