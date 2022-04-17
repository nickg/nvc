### Synopsis

[![Build
Status](https://github.com/nickg/nvc/workflows/Build%20and%20test/badge.svg?branch=master)](https://github.com/nickg/nvc/actions)
[![Coverage Status](https://coveralls.io/repos/github/nickg/nvc/badge.svg?branch=master)](https://coveralls.io/github/nickg/nvc?branch=master)

NVC is a [VHDL](https://en.wikipedia.org/wiki/VHDL) compiler and
simulator.

NVC supports almost all of VHDL-2002 and it has been successfully used
to simulate several real-world designs.  Experimental support for
VHDL-2008 is under development.

NVC has a particular emphasis on simulation performance and uses
[LLVM](https://llvm.org/) to compile VHDL to native machine code.

NVC is not a synthesizer.  That is, it does not output something that
could be used to program an FPGA or ASIC.  It implements only the
simulation behaviour of the language as described by the [IEEE
1076](https://standards.ieee.org/standard/1076-2019.html) standard.

### Usage

Simulating a VHDL hardware design involves three steps: _analysing_ the
source files; _elaborating_ the design; and _running_ the
simulation.  This is analogous to compiling, linking, and executing a
software program.  With NVC these steps are accomplished using the `-a`,
`-e`, and `-r` commands:

    $ nvc -a my_design.vhd my_tb.vhd
    $ nvc -e my_tb
    $ nvc -r my_tb

Or more succinctly, as a single command:

    $ nvc -a my_design.vhd my_tb.vhd -e my_tb -r

Where `my_tb` is the name of the top-level test-bench entity.

The full manual can be read after installation using `man nvc` or
[online](https://www.nickg.me.uk/nvc/manual.html).

### License

This program is [free
software](https://www.gnu.org/philosophy/free-sw.en.html) distributed
under the terms of the GNU General Public License version 3 or later.
You may use, modify, and redistribute the program as you wish but if you
distribute modifications you must preserve the license text and
copyright notices, and also make the modified source code available to
your users.

The source files for the IEEE standard libraries are included in the
repository.  These were originally provided under a proprietary license
that forbid distribution of modifications, but in 2019 were relicensed
under Apache 2.0.  Freely redistributable versions of the 1993 libraries
were made by editing and removing declarations from the 2019 libraries,
and so are also licensed under Apache 2.0.  Certain VHDL libraries
developed specifically for NVC under `lib/nvc` and `lib/std` are also
licensed under Apache 2.0.  See the individual files for details.

The VITAL libraries are distributed under `lib/vital`.  These were
derived from draft copies of the packages freely available on the
internet.  The license status of these is unclear as the final text is
part of the VITAL standard which must be purchased from the IEEE.  If
you are packaging this program for a distribution with strict free
software requirements you should strip these files from the tarball and
configure with `--disable-vital`.

### Installing

NVC is developed under GNU/Linux and is regularly tested on macOS and
Windows under MSYS2.

On macOS NVC can be installed with `brew install nvc`.  NVC is also
packaged for [FreeBSD](https://www.freshports.org/cad/nvc), [GNU
Guix](https://guix.gnu.org/packages/nvc-1.5.3/), and Arch Linux
[AUR](https://aur.archlinux.org/packages/nvc).  Users of other systems
should build from source.

NVC has both a release branch and a development master branch. The
master branch should be stable enough for day-to-day use and has
comprehensive regression tests, but the release branch is more suitable
for third party packaging.  The latest released version is
[1.6.2](https://github.com/nickg/nvc/releases/download/r1.6.2/nvc-1.6.2.tar.gz).
Significant changes since the last release are detailed in
[NEWS.md](NEWS.md).

To build from a Git clone:

    ./autogen.sh
    mkdir build && cd build
    ../configure
    make
    sudo make install

Generating the configure script requires autoconf 2.63 and automake 1.11
or later.

To build from a released tarball:

    ./configure
    make
    sudo make install

To use a specific version of LLVM add `--with-llvm=/path/to/llvm-config`
to the configure command.  The minimum supported LLVM version is 7.0.
Versions 7, 8, 9, 10, 11 and 12 have all been tested.

On Linux the `libdw` or `libdwarf` libraries can be used to generate
more accurate VHDL stack traces if installed.

NVC also depends on Flex to generate the lexical analyser.

On a Debian derivative the following should be sufficient to install all
required dependencies:

    sudo apt-get install build-essential automake autoconf \
      flex check llvm-dev pkg-config zlib1g-dev libdw-dev

Only the MSYS2 environment on Windows is supported.  The required
dependencies can be installed with:

    pacman -S base-devel mingw-w64-x86_64-{llvm,ncurses,libffi,check,pkg-config}

[GtkWave](http://gtkwave.sourceforge.net/) can be used to view
simulation waveforms.  Version 3.3.79 or later is required for the
default FST format.

#### Testing

To run the regression tests:

    make check

The unit tests require the [check](https://libcheck.github.io/check/)
library.

### Reporting bugs

Report bugs to [nick@nickg.me.uk](mailto:nick+nvc@nickg.me.uk) or using
the [GitHub issue tracker](https://github.com/nickg/nvc/issues).  Please
include enough information to reproduce the problem, ideally with a
small VHDL test case.  Issue
[#412](https://github.com/nickg/nvc/issues/412) is a good example.

Please remember that this software is provided to you with NO WARRANTY
and no expectation of support, but I will do my best to help with any
issues you encounter.

### Contributing

Thank your for your interest, but please note that at this time I am not
looking for additional regular contributors, nor do I have the time to
review large new features contributed by third parties.  That said I am
happy to accept patches to fix minor bugs, build issues, documentation,
etc.  Patches can be sent with either [git
--send-email](https://git-send-email.io/) or as a pull request on
GitHub.

If you are using NVC for your work or hobby project please get in
touch: all feedback is greatly appreciated.

### Language Support

VHDL standard revisions are commonly referred to by the year they were
published.  For example IEEE 1076-2008 is known as VHDL-2008.  The
default standard in NVC is VHDL-93 as this remains the most widely used
in the industry.  The default can be changed with the `--std` argument.
For example `--std=2008` selects the VHDL-2008 standard.

The 1993, 2000, and 2002 revisions of the standard are fully supported.
Please raise bugs for any missing or incorrectly implemented features
you encounter.

##### VHDL-2008

VHDL-2008 was a large change to the standard with many new features.
Support for these is currently in development.  Notable omissions
include:

* Enhanced generics on packages and subprograms are not supported.
* PSL is not supported.
* Hierarchical references are not supported.
* Force / release is not supported.
* `case?` statement with "don't care" is not supported.

##### VHDL-2019

The most recent revision of the standard also added many new features.
None of these are supported with the exception of conditional
compilation directives.

##### VHPI

The VHDL standard contains a comprehensive API called VHPI for
interfacing with foreign code written in C or another language. NVC
currently has very limited support for VHPI. Refer to the
[manual](https://www.nickg.me.uk/nvc/manual.html#VHPI) for more
information.

### Vendor Libraries

NVC provides scripts to compile the simulation libraries of common FPGA vendors.

* For Xilinx ISE use `./tools/build-xilinx-ise.rb`
* For Xilinx Vivado use `./tools/build-xilinx-vivado.rb`
* For Altera Quartus use `./tools/build-altera.rb`
* For Lattice iCEcube2 use `./tools/build-lattice.rb`

The libraries will be installed under `~/.nvc/lib`.
