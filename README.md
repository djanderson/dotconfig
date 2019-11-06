# Dotfiles

## CCLS

ccls >>> cquery, but ccls can't be built on Ubuntu 16.04 without some work. Here's the process that works for me:

### Update cmake to > 3.8

Instructions based on https://askubuntu.com/a/1157132.

- `sudo apt purge --auto-remove cmake`
- `wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null | sudo apt-key add -`
- `sudo apt-add-repository 'deb https://apt.kitware.com/ubuntu/ xenial main'`
- `sudo apt update && sudo apt install cmake`
```
cmake --version
cmake version 3.15.2
```

### Upgrade to gcc-7

This will bring in the required libstdc++-7 as well. Instructions based on https://gist.github.com/jlblancoc/99521194aba975286c80f93e47966dc5.

Install gcc-7 packages:

```
sudo apt-get install -y software-properties-common
sudo add-apt-repository ppa:ubuntu-toolchain-r/test
sudo apt update
sudo apt install g++-7 -y
```

Set it up so the symbolic links gcc, g++ point to the newer version:

`sudo apt-get install -y software-properties-common python-software-properties`

```
sudo apt-get install -y software-properties-common
sudo add-apt-repository ppa:ubuntu-toolchain-r/test
sudo apt update
sudo apt install g++-7 -y
```

```
sudo update-alternatives \
    --install /usr/bin/gcc gcc /usr/bin/gcc-7 60 \
    --slave /usr/bin/gcc-ar gcc-ar /usr/bin/gcc-ar-7 \
    --slave /usr/bin/gcc-nm gcc-nm /usr/bin/gcc-nm-7 \
    --slave /usr/bin/gcc-ranlib gcc-ranlib /usr/bin/gcc-ranlib-7 \
    --slave /usr/bin/g++ g++ /usr/bin/g++-7
```

Ensure that all version are >= 7.2

```
gcc --version
gcc (Ubuntu 7.4.0-1ubuntu1~16.04~ppa1) 7.4.0

g++ --version
g++ (Ubuntu 7.4.0-1ubuntu1~16.04~ppa1) 7.4.0
```

### Build ccls

Follow ccls build instructions, but use the clang+llvm binary for 16.04.

```
git clone --depth=1 --recursive https://github.com/MaskRay/ccls
cd ccls
wget -c http://releases.llvm.org/8.0.0/clang%2bllvm-8.0.0-x86_64-linux-gnu-ubuntu-16.04.tar.xz
tar xf clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-16.04.tar.xz
cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=$PWD/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-16.04
cmake --build Release
```

### Point ccls-executable to Release/ccls
