# helm-gtags
gtags tags different constructs in source-code and allows fast navigation

```bash
# build gtags from source because deb package on ubuntu is to old
sudo apt install libncurses5-dev id-utils exuberant-ctags
cd /tmp/
wget https://ftp.gnu.org/pub/gnu/global/global-6.6.3.tar.gz
tar -xvzf global-6.6.3.tar.gz
cd global-6.6.3
./configure --prefix=$HOME/global-6.6.3 --with-exuberant-ctags=/usr/bin/ctags-exuberant
make && make install
# I could not find a way to set the path to gtags in emacs
PATH=$PATH:~/global-6.6.3
```

# create tags for a project

```bash
cd path/to/project/root
gtags --gtagslabel=new-ctags
```
