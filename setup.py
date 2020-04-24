#!/usr/bin/python3

import os, json, subprocess
from shutil import which

home_path = os.environ['HOME']
emacsd_path = home_path + '/.emacs.d'
emacs_config_path = os.getcwd()

# programs, which will installed via apt package manager
apt_progs = ['xclip', 'xdotool' ,'aspell-de', 'aspell-en', 'gnutls-bin']

def install():
    """Run all steps, which are necessary to install a fresh emacs

    :returns:
    :rtype:

    """
    # link .emacs
    print('link .emacs: ln -s ' + emacs_config_path + '/.emacs ' + home_path + '/.emacs\n')
    os.symlink(emacs_config_path + '/.emacs', home_path + '/.emacs')

    # create ~/.emacs.d
    if not os.path.exists(emacsd_path):
        print('create folder: ' + emacsd_path)
        os.makedirs(emacsd_path)

    # run package install via apt
    install_apt()

    # set melpa package install to true
    print('set: (setq run-melpa t)\n')
    f_run_melpa = open(emacsd_path + '/run_melpa.el', 'w')
    f_run_melpa.write('(setq run-melpa t)\n')
    f_run_melpa.close()

    # link folder lisp
    print('link /lisp: ln -s '+ emacs_config_path + '/lisp ' + emacsd_path + '/lisp\n')
    os.symlink(emacs_config_path + '/lisp', emacsd_path + '/lisp')

    # create ~/.emacs.d/modes
    if not os.path.exists(emacsd_path + '/modes'):
        print('create folder: ' + emacsd_path + '/modes')
        os.makedirs(emacsd_path + '/modes')

    # search after modes in folder modes and ask for installation
    print('link *.el from folder modes')
    if os.path.exists(emacs_config_path + '/modes.json'):
        with open(emacs_config_path + '/modes.json', 'r+') as f_json:
            modes_json = json.load(f_json)
            install_modes(modes_json)
            f_json.seek(0)
            json.dump(modes_json, f_json, indent=2)
            f_json.truncate()
    else:
        with open(emacs_config_path + '/modes.json', 'w') as f_json:
            modes_json = {}
            install_modes(modes_json)
            json.dump(modes_json, f_json, indent=2)

def update():
    """Run all steps, to update the current emacs configuration.
       Should be performed when a new git commit is available.

    :returns:
    :rtype:

    """
    # run package install via apt
    install_apt()

    # set melpa package install to true
    print('set: (setq run-melpa t)\n')
    f_run_melpa = open(emacsd_path + '/run_melpa.el', 'w')
    f_run_melpa.write('(setq run-melpa t)\n')
    f_run_melpa.close()

    # search after modes in folder modes and ask for installation
    with open(emacs_config_path + '/modes.json', 'r+') as f_json:
            modes_json = json.load(f_json)
            install_modes(modes_json)
            f_json.seek(0)
            json.dump(modes_json, f_json, indent=2)
            f_json.truncate()

def install_apt():
    """Install all packages from the list apt_progs via 'apt' package manager of the system.

    :returns:
    :rtype:

    """
    if which('apt') is not None:
        apt_string = 'sudo apt install -y ' + ' '.join(apt_progs)
        print('run: ' + apt_string)
        process = subprocess.Popen(apt_string.split(), stdout=subprocess.PIPE)
        output, error = process.communicate()
        if output != b'' :
            print(output.decode("utf-8"))
        if error is not None:
            print('error: ' + str(error))
    else:
        print('apt package manager not found. Please install following Packages manually: ', end='')
        print(*progs)
    print('')

# modes_json: dict
def install_modes(modes_json):
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
    :returns:
    :rtype:

    """
    for (dirpath, dirnames, filenames) in os.walk(emacs_config_path + '/modes'):
        answer = None
        for dirname in dirnames:
            if dirname not in modes_json:
                answer = ''
                while answer not in ('y', 'n'):
                    # color codes for green on black background and white on black
                    answer = input('install ' + cs(dirname, 'Light Green')  + ' [y/n] : ')
                    if answer == 'y':
                        print('install')
                        for (subdirpath, subdirnames, subdirfilenames) in os.walk(dirpath + '/' + dirname):
                            print('link: ln -s '
                                  + subdirpath + '/' + dirname + '.el ' +
                                  emacsd_path + '/modes/' + dirname + '.el')
                            os.symlink(subdirpath + '/' + dirname + '.el',
                                       emacsd_path + '/modes/' + dirname + '.el')
                            modes_json[dirname] = "true"
                    elif answer == 'n':
                        print('skip install')
                        modes_json[dirname] = "false"
                    else:
                        print('Please enter y or n.')
        break

def cs(text : str, color: str):
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
        return text;

    output = ''
    if(color == 'Red'):
        output += '\033[0;31m'
    elif (color == 'Green'):
        output += '\033[0;32m'
    elif (color == 'Orange'):
        output += '\033[0;33m'
    elif (color == 'Blue'):
        output += '\033[0;34m'
    elif (color == 'Purple'):
        output += '\033[0;35m'
    elif (color == 'Cyan'):
        output += '\033[0;36m'
    elif (color == 'Light Gray'):
        output += '\033[0;37m'
    elif (color == 'Dark Grey'):
        output += '\033[1;30m'
    elif (color == 'Light Red'):
        output += '\033[1;31m'
    elif (color == 'Light Green'):
        output += '\033[1;32m'
    elif (color == 'Yellow'):
        output += '\033[1;33m'
    elif (color == 'Light Blue'):
        output += '\033[1;34m'
    elif (color == 'Light Purple'):
        output += '\033[1;35m'
    elif (color == 'Light Cyan'):
        output += '\033[1;36m'
    elif (color == 'White'):
        output += '\033[1;37m'

    return output + text + '\033[0m'

if __name__ == '__main__':
    # the file modes.json is an indicator that the initial installation has
    # taken place
    if os.path.exists(emacs_config_path + '/modes.json'):
        update()
    else:
        install()
