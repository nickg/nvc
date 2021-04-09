### Synopsis

[![Build
Status](https://github.com/nickg/nvc/workflows/Build%20and%20test/badge.svg?branch=master)](https://github.com/nickg/nvc/actions)
[![Coverage Status](https://coveralls.io/repos/github/nickg/nvc/badge.svg?branch=master)](https://coveralls.io/github/nickg/nvc?branch=master)

NVC is a GPLv3 VHDL compiler and simulator aiming for IEEE 1076-2002 compliance. See
these [blog posts](http://www.doof.me.uk/category/vhdl/) for background
information. NVC has been successfully used to simulate several real-world designs.

Brief usage example:

    $ nvc -a my_design.vhd my_tb.vhd
    $ nvc -e my_tb
    $ nvc -r my_tb

Or more succinctly, as a single command:

    $ nvc -a my_design.vhd my_tb.vhd -e my_tb -r

The full manual can be read after installing NVC using `man nvc` or
[online](nvc.1.md).

Report bugs using the [GitHub issue tracker](https://github.com/nickg/nvc/issues).

### Installing

NVC is developed on Debian Linux and is regularly tested on OS X and Windows
under MSYS2. Ports to other systems are welcome.

NVC has both a release branch and a development master branch. The master branch
should be stable enough for day-to-day use and has comprehensive regression tests,
but the release branch is more suitable for third party packaging. The latest
released version is
[1.5.1](https://github.com/nickg/nvc/releases/download/r1.5.1/nvc-1.5.1.tar.gz).
Significant changes since the last release are detailed in [HISTORY.md](HISTORY.md).

To build from a Git clone:

    ./autogen.sh
    mkdir build && cd build
    ../configure
    make
    make install

Generating the configure script requires autoconf 2.63 and automake 1.11 or later.

To build from a released tarball:

    ./configure
    make
    sudo make install

To use a specific version of LLVM add `--with-llvm=/path/to/llvm-config` to
the configure command. LLVM 7, 8, and 9 have all been tested.

NVC also depends GNU Flex to generate the lexical analyser.

If a readline-compatible library is installed it will be used to provide
line editing in the interactive mode.

[GtkWave](http://gtkwave.sourceforge.net/) can be used to view simulation
waveforms. Version 3.3.79 or later is required for the default FST format.

#### Debian and Ubuntu

On a Debian derivative the following should be sufficient to install all
required dependencies:

    sudo apt-get install build-essential automake autoconf autoconf-archive flex \
        check llvm-dev pkg-config zlib1g-dev

#### Mac OS X

The easiest way to install NVC on OS X is with [Homebrew](http://brew.sh/).

    brew install nvc

This will install the latest stable version. The current git master can be
installed with

    brew install --HEAD nvc

To build from source follow the generic instructions above.

#### Windows

Windows support is via MinGW or [Cygwin](http://www.cygwin.com/).

If you do not already have Cygwin it is easiest to build for MinGW using
[MSYS2](https://msys2.github.io/). Install the following dependencies using
`pacman`.

    pacman -S base-devel mingw-w64-x86_64-{llvm,ncurses,libffi,check,pkg-config}
    export PATH=/mingw64/bin:$PATH
    mkdir build && cd build
    ../configure
    make install

For Cygwin use `setup.exe` to install either `gcc` or `clang` and the following
dependencies: `automake`, `autoconf`, `pkg-config`, `llvm`, `libllvm-devel`,
`flex`, `libffi-devel`, `libcurses-devel`, and `make`. Then follow the
standard installation instructions above.

#### OpenBSD

Install the dependencies with `pkg_add`:

    pkg_add automake autoconf libexecinfo llvm check libexecinfo

To configure using the system compiler:

    ./configure LDFLAGS="-L/usr/local/lib" CC=/usr/bin/cc CXX=/usr/bin/c++ \
        --with-llvm=/usr/local/bin/llvm-config

Then follow the generic instructions above.

#### IEEE Libraries

The source files for the IEEE standard libraries are included in the repository
for convenience but the license forbids distribution of modifications. See
[lib/ieee/README.ieee](lib/ieee/README.ieee) for details. These files can also
be downloaded from the [IEEE
SA](https://standards.ieee.org/content/dam/ieee-standards/standards/web/download/).

To recompile the standard libraries:

    make bootstrap

Note this happens automatically when installing.

#### Testing

To run the regression tests:

    make check

The unit tests require the [check](http://check.sourceforge.net) library.

### VHDL-2008

NVC supports a small subset of VHDL-2008 which can be enabled with the `--std=2008`
option. If you require library functions from the 2008 standard you can use the
[VHDL-2008 Support Library](http://www.eda.org/fphdl/) which provides
backwards-compatible implementations for VHDL-1993. Run
`./tools/build-2008-support.rb` to download and install this.

### Vendor Libraries

NVC provides scripts to compile the simulation libraries of common FPGA vendors.
 * For Xilinx ISE use `./tools/build-xilinx-ise.rb`
 * For Xilinx Vivado use `./tools/build-xilinx-vivado.rb`
 * For Altera Quartus use `./tools/build-altera.rb`
 * For Lattice iCEcube2 use `./tools/build-lattice.rb`

The libraries will be installed under `~/.nvc/lib`.
