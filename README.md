### Synopsis

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

To build from a Git clone:

    ./autogen.sh
    ./tools/fetch-ieee.sh
    mkdir build && cd build
    ../configure
    make
    make install

Generating the configure script requires autoconf and automake
version 1.11 or later.

To use a specific version of LLVM add `--with-llvm=/path/to/llvm-config`
to the configure command. LLVM 3.0 or later is required.

NVC also depends GNU Flex to generate the lexical analyser.

If a readline-compatible library is installed it will be used to provide
line editing in the interactive mode.

[GtkWave](http://gtkwave.sourceforge.net/) can be used to view simulation
waveforms. Version 3.3.53 or later is reqiured for the default FST format.

#### Debian and Ubuntu

On a Debian derivative the following should be sufficient to install all required
dependencies:

    sudo apt-get install build-essential automake autoconf autoconf-archive flex \
        libreadline-dev tcl-dev check llvm-dev pkg-config zlib1g-dev curl

#### Mac OS X

This covers recent releases of OS X (10.9.X).

Install the Developer Command Line Tools. Either by installing Xcode and menu pull
down to install Command Line Tools or by Developer download of the Developer Command
Line Tools. This is essential for getting the include tree and making libraries, etc.
available.

Get LLVM from llvm.org. Executable images found at
http://llvm.org/releases/download.html.  The most recent version will likely work,
need at least LLVM 3.3.  The target name would be for example
`Clang_for_x86_64_Darwin_10.9`.

Install autotools. Either via Macports, Homebrew or directly.  Insure these are in
your path.

NVC installation is as described above with narrow exceptions.

Consider passing `--with-llvm=<absolute_path_to_llvm_bin_directory>` as a
command line argument to configure.  Apple provides `clang` and `clang++` as
well as `gcc` and `g++` linked to them but no `llvm-config`, etc.

This also implies you could manipulate your execution search path to use
`clang` and `clang++` from your added LLVM distribution to use as CC and CXX:

    configure CXX=clang++ CC=clang

The consequence of this is that LLVM is more strictly C99/C10/C11 compliant
while development of NVC is generally done with GNU GCC with more relaxed
compliance constraints.

#### Windows

Windows support is via [Cygwin](http://www.cygwin.com/). Use `setup.exe` to install
either `gcc` or `clang` and the following dependencies: `automake`, `autoconf`,
`pkg-config`, `llvm`, `libllvm-devel`, `flex`, `tcl`, `libreadline-devel`,
`libffi-devel`, `libcurses-devel`, and `make`. Then follow the standard installation
instructions above.

#### OpenBSD

TODO

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

You may need to install additional Ruby libraries:

    gem install colorize getopt

### VHDL-2008

NVC supports a small subset of VHDL-2008. If you require library functions from the
2008 standard you can use the [VHDL-2008 Support Library](http://www.eda.org/fphdl/)
which provides backwards-compatible implementations for VHDL-1993. Run
`./tools/build-2008-support.rb` to download and install this.

### Vendor Libraries

NVC provides scripts to compile the simulation libraries of common FPGA vendors.
 * For Xilinx ISE use `./tools/build-xilinx.rb`
 * For Altera Quartus use `./tools/build-altera.rb`
 * For Lattice iCEcube2 use `./tools/build-lattice.rb`

The libraries will be installed under `~/.nvc/lib`.
