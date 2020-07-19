#!/usr/bin/python3

import os, json, subprocess
from shutil import which
from typing import Dict, List
from helper import Conda_Handler, cs, answer_loop, Modes_Config_Handler


home_path = os.environ["HOME"]
emacsd_path = home_path + "/.emacs.d"
emacs_config_path = os.getcwd()

# These programs cannot be installed via conda or pip and must be installed via the system package manager.
# The setup tries to disable the associated Emacs functionality
#
# Structure:
# {<exe> : {"<value>"}}
# <value = "comment" : "<text>"
#        = "lisp-true" : "<lisp code>"
#        = "lisp-false" : "<lisp code>"
#
# comment: is printed if exe was not found and added to the setup_generated.el
# lisp-true: add code to the setup_generated.el if exe was found
# lisp-false: add code to the setup_generated.el if exe was not found
optinal_progs = {
    "xclip": {
        "comment": "Without xclip copying the clipboard between Emacs and X11 is not possible.",
        "lisp-true": "(setq enable_xclip 1)",
        "lisp-false": "(setq enable_xclip 0)",
    },
    "xdotool": {
        "comment": "Without xdotool, the sphinx-mode cannot refresh the web browser."
    },
    "aspell-de": {"comment": "The German dictionary is not available at flycheck."},
    "aspell-en": {
        "comment": "The English dictionary is not available at flycheck.",
        "lisp-true": "(setq flyspell-enable 1)",
        "lisp-false": "(setq flyspell-enable 0)",
    },
    "gnutls-bin": {
        "comment": "Required for the https connection to the packet servers."
    },
}


class Emacs_Setup:
    def __init__(self):
        """Emacs_Setup contains all functions to install and update my Emacs
        configuration.

        """
        # The content of the list is written to ~/.emacs.d/setup_generated.el
        # The file is loaded, befor local-vars.el
        self.setup_generated_el: List[str] = []

    def setup_conda(self, conda_env_name: str, python_version: str):
        """Creates a Conda_Handler and checks whether the environment with the
        name of the variable conda_env_name already exists. If not, asks whether
        it should be created.

        :param conda_env_name: Name of the environment.
        :type conda_env_name: str
        :param python_version: Environment Python version
        :type python_version: str

        :raises RuntimeError: Thrown if it is not possible to create a
                              Conda_Handler because, for example, no conda was
                              found.

        """
        try:
            self.conda_handler = Conda_Handler(
                conda_env_name=conda_env_name, python_version=python_version
            )
        except RuntimeError as e:
            print(e)
            exit(1)

        if not self.conda_handler.get_conda_env():
            print("conda environment does not exists")

            # temporary function for the function answer_loop, in case of answer 'no'
            def no_env():
                print("skip creating environment")
                exit(0)

            answer_loop(
                question="create environment",
                yes_func=self.conda_handler.create_conda_env,
                no_func=no_env,
            )

        print("conda env path: " + self.conda_handler.get_conda_env())

    def install(self):
        """Run all steps necessary to install a new Emacs. The function will
        fail if it has already been executed in the past. The modes are handled
        by update() only.

        """
        # link .emacs
        print(
            "link .emacs: ln -s "
            + emacs_config_path
            + "/.emacs "
            + home_path
            + "/.emacs\n"
        )
        os.symlink(emacs_config_path + "/.emacs", home_path + "/.emacs")

        # create ~/.emacs.d
        if not os.path.exists(emacsd_path):
            print("create folder: " + emacsd_path)
            os.makedirs(emacsd_path)

        # link folder lisp
        print(
            "link /lisp: ln -s "
            + emacs_config_path
            + "/lisp "
            + emacsd_path
            + "/lisp\n"
        )
        os.symlink(emacs_config_path + "/lisp", emacsd_path + "/lisp")

        # create ~/.emacs.d/modes
        if not os.path.exists(emacsd_path + "/modes"):
            print("create folder: " + emacsd_path + "/modes")
            os.makedirs(emacsd_path + "/modes")

        # create a empty modes.json file in the emacs-config file
        print("create modes.json")
        if not os.path.exists(emacs_config_path + "/modes.json"):
            with open(emacs_config_path + "/modes.json", "w") as f_json:
                json.dump({}, f_json, indent=2)

    def update(self):
        """Run all steps, to update the current emacs configuration. Should be
        performed when a new git commit is available.

        """
        # check if certain system packages are available
        self.__check_optinal_progs()

        # set melpa package install to true
        print("set: (setq run-melpa t)\n")
        f_run_melpa = open(emacsd_path + "/run_melpa.el", "w")
        f_run_melpa.write("(setq run-melpa t)\n")
        f_run_melpa.close()

        self.setup_generated_el.append(";; setup conda path for conda.el")
        self.setup_generated_el.append(
            '(setq conda-anaconda-home "{0}")\n'.format(self.conda_handler.conda_path)
        )

        # search after modes in folder modes and ask for installation
        with open(emacs_config_path + "/modes.json", "r+") as f_json:
            modes_json = json.load(f_json)
            self.__install_modes(modes_json)
            f_json.seek(0)
            json.dump(modes_json, f_json, indent=2)
            f_json.truncate()

        # search for config.json files in the modes folders and execute it
        for mode in modes_json:
            if modes_json[mode] == "true":
                self.__update_modes(mode)

        # generates the ~/.emacs.d/setup_generated.el
        self.__generate_lisp_code()

    def __check_optinal_progs(self):
        """Checks if an executable file is available on the system. Depending on
        the command, add some lisp code to setup_generated.el to process it.

        """
        for prog in optinal_progs:
            # use apt check, if package is installed
            check_process = subprocess.Popen(
                ["apt", "-qq", "list", prog],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            output, error = check_process.communicate()
            if "[installed]" in output.decode("utf-8"):
                print(cs("found " + prog, "Green"))
                # Lisp code for the exe, which was found
                if "lisp-true" in optinal_progs[prog]:
                    self.setup_generated_el.append(
                        ";; " + optinal_progs[prog]["comment"]
                    )
                    self.setup_generated_el.append(optinal_progs[prog]["lisp-true"])
            else:
                print(
                    cs(
                        "could not found "
                        + prog
                        + "\n"
                        + optinal_progs[prog]["comment"],
                        "Red",
                    )
                )
                # Lisp code for the exe, which was not found
                if "lisp-false" in optinal_progs[prog]:
                    self.setup_generated_el.append(
                        ";; " + optinal_progs[prog]["comment"]
                    )
                    self.setup_generated_el.append(optinal_progs[prog]["lisp-false"])
        # empty line, to separate this section in the setup-generated.el file
        self.setup_generated_el.append("\n")

    def __install_modes(self, modes_json: Dict):
        """Handles the modes that are not part of the default configuration. For
           each mode, the tool queries the decision and saves it in the modes.json
           file. If the answer was positive, the .el is linked to the .emacs.d/modes
           folder and is automatically loaded by emacs at startup. Sometimes some
           additional steps are necessary, which are described in the README.md in
           the corresponding folder.

        :param modes_json: Dict, which contains a json file. sed to save when a mode
                           has already been handled. The input must be at least an
                           empty dictation.
        :type modes_json: dict

        """
        for (dirpath, dirnames, filenames) in os.walk(emacs_config_path + "/modes"):
            answer = None
            for dirname in dirnames:
                if dirname not in modes_json:

                    def yes_mode():
                        print("install")
                        for (subdirpath, subdirnames, subdirfilenames) in os.walk(
                            dirpath + "/" + dirname
                        ):
                            print(
                                "link: ln -s "
                                + subdirpath
                                + "/"
                                + dirname
                                + ".el "
                                + emacsd_path
                                + "/modes/"
                                + dirname
                                + ".el"
                            )
                            os.symlink(
                                subdirpath + "/" + dirname + ".el",
                                emacsd_path + "/modes/" + dirname + ".el",
                            )

                            modes_json[dirname] = "true"

                    def no_mode():
                        print("skip install")
                        modes_json[dirname] = "false"

                    answer_loop(
                        question="install " + cs(dirname, "Light Green"),
                        yes_func=yes_mode,
                        no_func=no_mode,
                    )
            break

    def __update_modes(self, mode: str):
        """Searches for config.json files for each enabled mode. If a file is
        found, install the packages and set the lisp code.

        :param mode: Name of the mode.
        :type mode: str

        """
        config_path = emacs_config_path + "/modes/" + mode + "/config.json"
        print(
            cs(
                "#######################################################################\n"
                + "# Found config {0} \n".format(config_path)
                + "#######################################################################\n",
                "Blue",
            )
        )
        if os.path.exists(config_path):
            print(cs("found config: " + config_path, "Yellow"))
            mch = Modes_Config_Handler(config_path)
            # install conda packages
            mch.install_conda_packages(self.conda_handler)
            # add lisp code to the setup_generated.el
            mch.set_lisp(self.setup_generated_el, self.conda_handler.get_conda_env())

    def __generate_lisp_code(self):
        """Write the content of setup_generated_el to the file
        ~/.emacs.d/setup_generated.el

        """
        with open(emacsd_path + "/setup_generated.el", "w") as f:
            f.write(
                ";; Do note edit this file. It is generated by the setup.py and will be overwritten.\n\n"
            )
            for line in self.setup_generated_el:
                f.write(line)
                f.write("\n")


if __name__ == "__main__":

    es = Emacs_Setup()
    es.setup_conda(conda_env_name="emacs-tools", python_version="3.8")

    # the file modes.json is an indicator that the initial installation has
    # taken place
    if not os.path.exists(emacs_config_path + "/modes.json"):
        es.install()

    es.update()
