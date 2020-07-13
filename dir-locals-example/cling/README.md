# About

Personal config to develop, build and run [cling](https://github.com/root-project/cling) in emacs. There are some helper functions defined in `modes/irony` to extend the `cmake` project type in projectle. The `cm-*` functions and variables allows to switch between debug and release build type and create for each build type a own build and install folder. There is also support for conda environments and singularity container.

The cling project has to special properties.

1. Cling is a interactive application. Therefore, it is necessary to use a shell in emacs. The `*-cling` functions create a shell and (re-)run the cling process in it.
2. The project contains 3 git repository. The `llvm` project is the root folder. The `cling` and `clang` repos are in the `tools` folder. It should be possible to navigate to each file and every `projectile` project (equal to the git repos) should build in the same folder (located in the `llvm` folder).

# Placement of the files

`.dir-locals.el` and `.projectile` must be placed in the `llvm/` folder.

# Variables

- **compilation-read-command**: if the value is `t` (default), projectile ask after the build or configure command, before it execute. `nil` skips the question.
- **cm-conda-env**: if the value is not empty, add prefix `conda run -n <cm-conda-env>` to the configure, build and run command
- **cm-build-dir**: cmake build folder
- **cm-install-dir**: CMAKE\_INSTALL\_PREFIX
- **cm-configure-cmd**: cmake configure arguments
- **cling-args**: add arguments to cling, when `run-cling` execute a instance
- **llvm-root**: save the folder of the .dir-locals.el, which is the root folder of the llvm project -> can be usesd for relative paths, independent of the file, which was open via projectile

# Functions

- **cm-set-projectile-project-type**: execute `projectile-register-project-type` with the cm-variables
- **init-cling-shell**: opens a ansi-term and activate the conda environment
- **rerun-cling**: Restarts the cling process and switch to the buffer. If the `*cling-shell*` buffer is visible, set the focus to it (works also with different frames). Otherwise opens the buffer in the current window.
- **run-cling**: Initial create ansi-term and (re-)run cling process
