### Synopsis

[![Build Status](https://github.com/nickg/nvc/actions/workflows/build-test.yml/badge.svg?branch=master&event=push)](https://github.com/nickg/nvc/actions)
[![Coverage Status](https://coveralls.io/repos/github/nickg/nvc/badge.svg?branch=master)](https://coveralls.io/github/nickg/nvc?branch=master)

NVC is a [VHDL](https://en.wikipedia.org/wiki/VHDL) compiler and
simulator.

NVC supports almost all of VHDL-2008 with the exception of PSL, and it
has been successfully used to simulate several real-world designs.
Experimental support for VHDL-2019 is under development.

NVC has a particular emphasis on simulation performance and uses
[LLVM](https://llvm.org/) to compile VHDL to native machine code.

NVC is not a synthesizer.  That is, it does not output something that
could be used to program an FPGA or ASIC.  It implements only the
simulation behaviour of the language as described by the [IEEE
1076](https://standards.ieee.org/standard/1076-2019.html) standard.

NVC supports popular verification frameworks including
[OSVVM](https://osvvm.org/), [UVVM](https://www.uvvm.org/),
[VUnit](https://vunit.github.io/) and [cocotb](https://www.cocotb.org/).
See [below](#vendor-libraries) for installation instructions.

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
packaged for [FreeBSD](https://www.freshports.org/cad/nvc),
[Gentoo](https://packages.gentoo.org/packages/sci-electronics/nvc), Arch
Linux [AUR](https://aur.archlinux.org/packages/nvc), and [several other
distributions](https://repology.org/project/nvc/versions).  A Windows
installer is available from the [releases
page](https://github.com/nickg/nvc/releases/) and can be installed using
[winget](https://learn.microsoft.com/en-us/windows/package-manager/winget/)
with `winget install NickGasson.NVC`.  Users of systems without existing
packages should build from source.

NVC has both a release branch and a development master branch. The
master branch should be stable enough for day-to-day use and has
comprehensive regression tests, but the release branch is more suitable
for third party packaging.  The latest released version is
[1.15.2](https://github.com/nickg/nvc/releases/tag/r1.15.2).
Significant changes since the last release are detailed in
[NEWS.md](NEWS.md).

If you are building from a Git clone rather than a released tarball you
first need to generate the configure script using:

    ./autogen.sh

In-tree builds are not supported so create a separate build directory:

    mkdir build && cd build

Finally build and install using the standard autotools steps:

    ../configure
    make
    sudo make install

To use a specific version of LLVM add `--with-llvm=/path/to/llvm-config`
to the configure command.  The minimum supported LLVM version is 8.0.
Versions between 8 and 18 have all been tested.

NVC also depends on Flex to generate the lexical analyser.

On a Debian derivative the following should be sufficient to install all
required dependencies:

    sudo apt-get install build-essential automake autoconf \
      flex check llvm-dev pkg-config zlib1g-dev libdw-dev \
      libffi-dev libzstd-dev

On `rpm` based distributions, the following can be installed to fulfill
required dependencies:

    sudo dnf install autoconf automake flex check llvm-devel libffi-devel \
      zlib-ng-compat-devel libzstd-devel elfutils-devel

Only the MSYS2 environment on Windows is supported.  The required
dependencies can be installed with:

    pacman -S base-devel mingw-w64-x86_64-{llvm,ncurses,libffi,check,pkg-config,zstd}

[GTKWave](http://gtkwave.sourceforge.net/) or [Surfer](https://surfer-project.org/)
can be used to view simulation waveforms.  For GTKWave, version 3.3.79 or later is
required for the default FST format.

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

Patches can be sent as either pull requests on GitHub or by email using
[git --send-email](https://git-send-email.io/).  Please note however
that as this is purely a spare-time hobby project for me, I have limited
time available to review patches.  All code submitted must follow the
guidelines in [contrib/STYLE.md](contrib/STYLE.md).

I will not accept patches that add new copyright owners under `src/`.
This is to ensure there is clear legal ownership should, for example,
the license need to be updated.  Significant contributors are instead
listed in [THANKS.md](THANKS.md).

### Language Support

VHDL standard revisions are commonly referred to by the year they were
published.  For example IEEE 1076-2008 is known as VHDL-2008.  The
default standard in NVC is currently VHDL-2008 but this can be changed
with the `--std` argument.  For example `--std=1993` selects the
VHDL-1993 standard.

The 1993, 2000, and 2002 revisions of the standard are fully supported.
Please raise bugs for any missing or incorrectly implemented features
you encounter.  The current status of VHDL-2008 and VHDL-2019 support
can be found on the [features](https://www.nickg.me.uk/nvc/features.html)
page.

##### VHPI

The VHDL standard contains a comprehensive API called VHPI for
interfacing with foreign code written in C or another language.  NVC
implements a subset of VHPI sufficient for running
[cocotb](https://www.cocotb.org/).  Refer to the
[manual](https://www.nickg.me.uk/nvc/manual.html#VHPI) for more
information.

### Vendor Libraries

NVC provides scripts to compile popular verification frameworks and the
simulation libraries of common FPGA vendors.

* For [OSVVM](https://osvvm.org/) use `nvc --install osvvm`
* For [UVVM](https://www.uvvm.org/) use `nvc --install uvvm`
* For Xilinx ISE use `nvc --install ise`
* For Xilinx Vivado use `nvc --install vivado` and additionally `nvc
  --install xpm_vhdl` if you require simulation models of the XPM macros
* For Altera Quartus use `nvc --install quartus`
* For Lattice iCEcube2 use `nvc --install icecube2`
* For [Free Model Foundry](https://freemodelfoundry.com/) common
  packages use `nvc --install fmf`

The libraries will be installed under `~/.nvc/lib`.
