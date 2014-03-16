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

The full manual can be read after installing NVC using `man nvc` or
[online](nvc.1.md).

Report bugs using the [GitHub issue tracker](https://github.com/nickg/nvc/issues).

### Installing

NVC is developped on Debian Linux and has been reported to work on OS X
and Windows under Cygwin. Ports to other Unix-like systems are welcome.

To build from a Git clone:

    ./autogen.sh
    ./tools/fetch-ieee.sh
    mkdir build && cd build
    ../configure
    make
    make install

Generating the configure script requires autoconf and automake
version 1.12 or later.

To use a specific version of LLVM add `--with-llvm=/path/to/llvm-config`
to the configure command. LLVM 3.0 or later is required.

NVC also depends on GNU Flex and Bison to generate the parser. The Bison version
should be at least 2.5.

If a readline-compatible library is installed it will be used to provide
line editing in the interactive mode.

#### Debian and Ubuntu

On a Debian derivative the following should be sufficient to install all required
dependencies:

    sudo apt-get install build-essential automake autoconf flex bison \
        libreadline-dev tcl-dev check llvm-dev

#### Mac OS X

TODO

#### Windows

Windows support is via [Cygwin](http://www.cygwin.com/).

TODO: dependencies

#### OpenBSD

TODO

#### IEEE Libraries

Due to copyright restrictions the IEEE library source files cannot be freely
redistributed and must be downloaded from an external source prior to building. See
[lib/ieee/README](lib/ieee/README) for details.

To recompile the standard libraries:

    make -C lib clean
    make bootstrap

Note this happens automatically when installing.

#### Testing

To run the regression tests:

    make check

The unit tests require the [check](http://check.sourceforge.net) library.

You may need to install additional Ruby libraries:

    gem install colorize getopt

### Vendor Libraries

NVC provides scripts to compile the simulation libraries of common FPGA vendors.
 * For Xilinx ISE use `./tools/build-xilinx.rb`
 * For Altera Quartus use `./tools/build-altera.rb`

The libraries will be installed under `~/.nvc/lib`.
