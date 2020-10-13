import os, json, subprocess
from typing import Dict, List


class Conda_Handler:
    def __init__(self, conda_env_name: str, python_version: str):
        """The Conda_Handler is responsible for finding the Conda package
        manager on the system, managing the Emacs Conda environment and
        installing new packages.

        :param conda_env_name: Name of the conda environment in which the
                               external Emacs tools live.
        :type conda_env_name: str
        :param python_version: Python version of the environment.
        :type python_version: str

        :raises RuntimeError: If Conda was not found or the Conda base path
                              cannot be extracted

        """
        self.conda_env_name = conda_env_name
        self.python_version = python_version

        # find Conda
        find_conda_process = subprocess.Popen(
            ["which", "conda"], stdout=subprocess.PIPE, stderr=subprocess.PIPE
        )
        output, error = find_conda_process.communicate()
        if output == b"":
            raise RuntimeError("conda not found, please add conda to your $PATH")
        else:
            self.conda_bin_path = output.decode("utf-8").replace("\n", "")

        # try to extract the Conda base path
        if not self.conda_bin_path.endswith(
            "/condabin/conda"
        ) and not self.conda_bin_path.endswith("/bin/conda"):
            raise RuntimeError(
                "Unknown miniconda bin path {0}\nThe conda exe must be located in the folder <miniconda3>/condabin/conda or <miniconda3>/bin/conda.".format(
                    self.conda_bin_path
                )
            )
        else:
            if self.conda_bin_path.endswith("/condabin/conda"):
                self.conda_path = self.conda_bin_path[: -len("/condabin/conda")]
            if self.conda_bin_path.endswith("/bin/conda"):
                self.conda_path = self.conda_bin_path[: -len("/bin/conda")]

        # create empty to path avoid checks, if variable is available
        self.conda_env_path = ""

    def get_conda_env(self) -> str:
        """Returns the root directory of Emacs Conda environment.

        :returns: root directory of Emacs Conda environment
        :rtype: str

        """
        if not self.conda_env_path:
            find_env_process = subprocess.Popen(
                ["conda", "run", "-n", self.conda_env_name, "echo", "$CONDA_PREFIX"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            output, error = find_env_process.communicate()
            if find_env_process.returncode == 0:
                self.conda_env_path = output.decode("utf-8").replace("\n", "")

        return self.conda_env_path

    def create_conda_env(self):
        """Create the Emacs Conda environment.

        :raises RuntimeError: If the environment already exists.

        """
        if self.get_conda_env():
            raise RuntimeError(
                "conda environment {} already exists".format(self.conda_env_name)
            )
        else:
            create_env_process = subprocess.Popen(
                [
                    "conda",
                    "create",
                    "-y",
                    "-n",
                    self.conda_env_name,
                    "python=" + self.python_version,
                ],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            output, error = create_env_process.communicate()
            print(output.decode("utf-8"))

    def conda_install(self, command: str) -> int:
        """Install package in the Emacs Conda environment.

        :param command: Package name. The channel and version number can appended.
        :type command: str
        :returns: The error code of conda install
        :rtype: int

        """
        if not self.get_conda_env():
            raise RuntimeError(
                "cannot install: conda environment {} does not exists".format(
                    self.conda_env_name
                )
            )
        else:
            create_env_process = subprocess.Popen(
                ["conda", "install", "-y", "-n", self.conda_env_name] + command.split(),
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            output, error = create_env_process.communicate()
            print(output.decode("utf-8"))
            return create_env_process.returncode

    def conda_run(self, command: str) -> int:
        """run command in the Emacs Conda environment.

        :param command: The command which follow after conda run -n <env>
        :type command: str
        :returns: The error code
        :rtype: int

        """
        if not self.get_conda_env():
            raise RuntimeError(
                "cannot install: conda environment {} does not exists".format(
                    self.conda_env_name
                )
            )
        else:
            create_env_process = subprocess.Popen(
                ["conda", "run", "-n", self.conda_env_name] + command.split(),
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            output, error = create_env_process.communicate()
            print(output.decode("utf-8"))
            return create_env_process.returncode


class Modes_Config_Handler:
    def __init__(self, path: str):
        """The Modes_Config_Handler reads the config.json and does a job for
        each entry.

        :param path: Path to the config.
        :type path: str

        """
        with open(path, "r") as f_json:
            self.json_config = json.load(f_json)

    def install_conda_packages(self, conda_handler: Conda_Handler):
        """If the key word "conda" is detected, install the packages.

        :param conda_handler: Conda Handler, which is used to install the packages.
        :type conda_handler: Conda_Handler

        """
        for k in self.json_config:
            package = self.json_config[k]
            if "conda" in package:
                for cmd in package["conda"]:
                    conda_handler.conda_install(cmd)
            if "pip" in package:
                for cmd in package["pip"]:
                    conda_handler.conda_run("pip3 install " + cmd)

    def set_lisp(self, lisp: List[str], conda_env_root: str):
        """If the key word "lisp" is detected, add lisp code to the lisp list.

        :param lisp: The list which the lisp commands are added.
        :type lisp: List[str]
        :param conda_env_root: Replace the placeholder <env_root> in the vars
                               section with conda_env_root. Should be the path
                               to the Emacs Conda environment.
        :type conda_env_root: str

        """
        for k in self.json_config:
            package = self.json_config[k]
            # transform comment text lisp comment and add to the top of the section
            if "comment" in package:
                lisp.append(";; " + package["comment"])
            # create lisp setq command and replace <env_root> placeholder
            if "vars" in package:
                for v in package["vars"]:
                    if isinstance(package["vars"][v], list):
                        # create a lisp list
                        s = "(setq {0} '(".format(v)
                        for e in package["vars"][v]:
                            s += '"{0}" '.format(e.replace("<env_root>", conda_env_root))
                        s = s.rstrip()
                        s += "))"
                        lisp.append(s)
                    else:
                        lisp.append(
                            '(setq {0} "{1}")\n'.format(
                                v, package["vars"][v].replace("<env_root>", conda_env_root)
                            )
                    )
            # add raw lisp code
            if "lisp" in package:
                for l in package["lisp"]:
                    lisp.append(l)
            lisp.append("\n")


def answer_loop(question, yes_func, no_func):
    """The function creates an interactive question-answer loop. If the answer
    is "y", execute the function yes_func. If the answer is "n", execute the
    function no_func. If the answer is something else, ask again.

    :param question: Queries text.
    :type question: str
    :param yes_func: Function, which is executed, if "y"
    :type yes_func: func
    :param no_func: Function, which is executed, if "n"
    :type no_func: func

    """
    answer = ""
    while answer not in ("y", "n"):
        # color codes for green on black background and white on black
        answer = input(question + " [y/n] : ")
        if answer == "y":
            yes_func()
        elif answer == "n":
            no_func()
        else:
            print("Please enter y or n.")


def cs(text: str, color: str):
    """Print the text in a different color on the command line. The text after
       the function has the default color of the command line.

    :param text: text to be colored
    :type text: str
    :param color: Name of the color. If wrong color or empty, use default color
                  of the command line.
    :type color: str
    :returns:
    :rtype:

    """
    if color is None:
        return text

    output = ""
    if color == "Red":
        output += "\033[0;31m"
    elif color == "Green":
        output += "\033[0;32m"
    elif color == "Orange":
        output += "\033[0;33m"
    elif color == "Blue":
        output += "\033[0;34m"
    elif color == "Purple":
        output += "\033[0;35m"
    elif color == "Cyan":
        output += "\033[0;36m"
    elif color == "Light Gray":
        output += "\033[0;37m"
    elif color == "Dark Grey":
        output += "\033[1;30m"
    elif color == "Light Red":
        output += "\033[1;31m"
    elif color == "Light Green":
        output += "\033[1;32m"
    elif color == "Yellow":
        output += "\033[1;33m"
    elif color == "Light Blue":
        output += "\033[1;34m"
    elif color == "Light Purple":
        output += "\033[1;35m"
    elif color == "Light Cyan":
        output += "\033[1;36m"
    elif color == "White":
        output += "\033[1;37m"

    return output + text + "\033[0m"
