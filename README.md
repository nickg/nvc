### Synopsis

[![Build Status](https://travis-ci.org/nickg/nvc.svg?branch=master)](https://travis-ci.org/nickg/nvc)
[![Join the chat at https://gitter.im/nvc-vhdl/Lobby](https://badges.gitter.im/nvc-vhdl/Lobby.svg)](https://gitter.im/nvc-vhdl/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

NVC is a GPLv3 VHDL compiler and simulator aiming for IEEE 1076-1993 compliance. See
these [blog posts](http://www.doof.me.uk/category/vhdl/) for background
information. NVC should not be considered a production quality tool and language
support is lacking in many areas. However it has been successfully used to simulate
several real-world designs.

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

NVC is developed on Debian Linux and has been reported to work on OS X
and Windows under Cygwin. Ports to other Unix-like systems are welcome.

NVC has both a release branch and a development master branch. The master branch
should be stable enough for day-to-day use and has comprehensive regression tests,
but the release branch is more suitable for third party packaging. The latest
released version is
[1.2.1](https://github.com/nickg/nvc/releases/download/r1.2.1/nvc-1.2.1.tar.gz). Significant
changes since the last release are detailed in [HISTORY.md](HISTORY.md).

To build from a Git clone:

    ./autogen.sh
    ./tools/fetch-ieee.sh
    mkdir build && cd build
    ../configure
    make
    make install

Generating the configure script requires autoconf 2.63 and automake 1.11 or later.

To build from a released tarball:

    ./tools/fetch-ieee.sh
    ./configure
    make
    sudo make install

To use a specific version of LLVM add `--with-llvm=/path/to/llvm-config`
to the configure command. LLVM 3.8 or later is required.

NVC also depends GNU Flex to generate the lexical analyser.

If a readline-compatible library is installed it will be used to provide
line editing in the interactive mode.

[GtkWave](http://gtkwave.sourceforge.net/) can be used to view simulation
waveforms. Version 3.3.53 or later is reqiured for the default FST format.

#### Debian and Ubuntu

On a Debian derivative the following should be sufficient to install all required
dependencies:

    sudo apt-get install build-essential automake autoconf autoconf-archive flex \
        check llvm-dev pkg-config zlib1g-dev curl

#### Mac OS X

The easiest way to install NVC on OS X is with [Homebrew](http://brew.sh/).

    brew install nvc

This will install the latest stable version. The current git master can be installed with

    brew install --HEAD nvc

To build from source follow the generic instructions above.

#### Windows

Windows support is via [Cygwin](http://www.cygwin.com/) or MSYS2.

MSYS2 is easier to set up if you do not already have Cygwin. Install the following
dependencies using `pacman`. For 64-bit MSYS2 replace `i686` below with `x86_64`.

    pacman -S base-devel mingw-w64-i686-{tcl,llvm,ncurses,libffi}
    export PATH=$PATH:/mingw32/bin/     # Or mingw64 for 64-bit
    mkdir build && cd build
    ../configure --with-llvm=/mingw32/bin/llvm-config.exe
    make install

For Cygwin use `setup.exe` to install either `gcc` or `clang` and the following
dependencies: `automake`, `autoconf`, `pkg-config`, `llvm`, `libllvm-devel`, `flex`,
`libffi-devel`, `libcurses-devel`, `curl`, and `make`. Then follow the standard
installation instructions above.

#### OpenBSD

Install the dependencies with `pkg_add`:

    pkg_add -r automake-1.15p0 autoconf-2.69p2 libexecinfo llvm check libexecinfo

Use a modern compiler such as Clang to build, as the system GCC does not support many modern
language features.

    ./configure --with-llvm=/usr/local/bin/llvm-config CC=clang CXX=clang++

Then follow the generic instructions above.

#### IEEE Libraries

Due to copyright restrictions the IEEE library source files cannot be freely
redistributed and must be downloaded from an external source prior to building. See
[lib/ieee/README](lib/ieee/README) for details.

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
