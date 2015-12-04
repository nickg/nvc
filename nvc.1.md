nvc(1) -- VHDL Compiler and Simulator
=====================================

## SYNOPSIS

`nvc` -a [_options_] _files_...<br>
`nvc` -e [_options_] _unit_<br>
`nvc` -r [_options_] _unit_<br>

## DESCRIPTION

NVC is an implementation of the VHDL language as defined by IEEE standard 1076-1993
and later revisions. Simulating a design typically involves three steps: analysing
one more more source files into the work library; elaborating a top-level design
unit; and running the elaborated design.

## OPTIONS

NVC accepts three kinds of options: global options; commands; and options specific to
the command. Global options must be placed before the command and specific options
must be placed after the command.

Commands can be chained together. For example to analyse a file `foo.vhd` and then
elaborate and run a top-level entity `bar`:

    nvc -a foo.vhd -e bar -r

Note that the _unit_ argument for the `-r` run command is taken from the earlier `-e`
elaborate command.

### Commands

 * `-a` _files_:
   Analyse _files_ into the work library.

 * `-e` _unit_:
   Elaborate a previously analysed top level design unit.

 * `-r` _unit_:
   Execute a previously elaborated top level design unit.

 * `--codegen` _unit_:
   Generate a native shared library for a previously analysed package. This
   can improve runtime performance if the package contains a large number of
   frequently used subprograms.

 * `--dump` _unit_:
   Print out a pseudo-VHDL representation of an analysed unit. This is
   usually only useful for debugging the compiler.

 * `--make` _units_:
   Generate a makefile for already analysed units.

### Global options

* `--force-init`:
  Initialise a library work directory even if it already exists and is non-empty.

 * `-h`, `--help`:
   Display usage summary.

 * `--ignore-time`:
   Do not check the timestamps of source files when the corresponding design unit is
   loaded from a library.

 * `-L` _path_:
   Add _path_ to the list of directories to search for libraries. See the
   [LIBRARIES][] section below for details.

* `--map=`_name_`:`_path_:
   Specify exactly the location of logical library _name_. Libraries mapped in this
   way will not used the normal search path.

 * `--messages=`_style_:
   Select either the _full_ or _compact_ message format. The default full message
   format is designed for readability whereas the compact messages can be easily
   parsed by tools.

 * `--std=`_rev_:
   Select the VHDL standard revision to use. Specify either the full year such as
   _1993_ or the decade such as _93_. The allowed revisions are 1993, 2000, 2002,
   and 2008. Note there is very limited supported for any features beyond those in
   VHDL-93. VHDL-87 is not supported.

 * `-v`, `--version`:
   Display version and copyright information.

 * `--work=`_name_, `--work=`_name_`:`_path_:
   Use _name_ as the work library. The second variant explicitly specifies the
   location of the library. See the [LIBRARIES][] section below for details.

### Analysis options

* `--bootstrap`:
  Allow compilation of the STANDARD package. Not intended for end users.

* `--relax=`_rules_:
  Disable certain pedantic rule checks specified in the comma-separate list
  _rules_. See [RELAXING RULES][] section below for full list.

### Elaboration options

* `--cover`:
  Enable code coverage reporting (see the [CODE COVERAGE][] section below).

* `--disable-opt`:
  Disable backend optimisations. Not generally useful unless debugging the
  generated LLVM IR.

* `--dump-llvm`:
  Print generated LLVM IR prior to optimisation.

* `--dump-vcode`:
  Print generated intermediate code.

* `-g` _name_`=`_value_:
  Override top-level generic _name_ name with _value_. Integers, enumeration
  literals, and string literals are supported. For example `-gI=5`, `-gINIT='1'`,
  and `-gSTR=hello`.

* `--native`:
  Generate native code shared library. By default NVC will use LLVM JIT
  compilation to generate machine code at runtime. For large designs
  compiling to native code at elaboration time may improve performance.

* `-V`, `--verbose`:
  Prints resource usage information after each elaboration step.

### Runtime options

 * `-b`, `--batch`:
   Run in batch mode. This is the default.

 * `-c`, `--command`:
   Run in interactive TCL command line mode. See [TCL SHELL][] section below.

 * `--exit-severity=`_level_:
   Terminate the simulation after an assertion failures of severity greater than
   or equal to _level_. Valid levels are `note`, `warning`, `error`, and `failure`.
   The default is `error`.

 * `--format=`_fmt_:
   Generate waveform data in format _fmt_. Currently supported formats are:
   `fst`, `lxt`, and `vcd`. The FST and LXT formats are native to GtkWave.
   The FST format is preferred over LXT due its smaller size and better
   performance; however VHDL support in FST requires a recent version of
   GtkWave so LXT is provided for compatibility. VCD is a very widely used
   format but has limited ability to represent VHDL types and the performance
   is poor: select this only if you must use the output with a tool that does
   not support FST or LXT. The default format is FST if this option is not
   provided. Note that GtkWave 3.3.53 or later is required to view the FST
   output.

 * `--include=`_glob_, `--exclude=`_glob_:
   Signals that match _glob_ are included in or excluded from the waveform
   dump. See section [SELECTING SIGNALS][] for details on how to select
   particular signals. These options can be given multiple times.

 * `--load=`_plugin_:
   Loads a VHPI plugin from the shared library _plugin_. See
   section [VHPI][] for details on the VHPI implementation.

 * `--stats`:
   Print time and memory statistics at the end of the run.

 * `--stop-delta=`_N_:
   Stop after _N_ delta cycles. This can be used to detect zero-time loops
   in your model. The default is 1000 if not specified. Setting this to
   zero disables the delta cycle limit.

 * `--stop-time=`_T_:
   Stop the simulation after the given time has elapsed. Format of _T_ is
   an integer followed by a time unit in lower case. For example `5ns` or
   `20ms`.

 * `--trace`:
   Trace simulation events. This is usually only useful for debugging the
   simulator.

 * `--vhpi-trace`:
   Trace VHPI calls and events. This can be useful for debugging VHPI plugins.

 * `-w, --wave=`_file_:
   Write waveform data to _file_. The file name is optional and if not specified
   will default to the name of the top-level unit with the appropriate extension
   for the waveform format. The waveform format can be specified with the
   `--format` option. By default all signals in the design will be dumped: see
   the [SELECTING SIGNALS][] section below for how to control this.

### Make options

 * `--deps-only`:
   Generate rules that only contain dependencies without actions. These can be
   useful for inclusion in a hand written makefile.

 * `--native`:
   Output actions to generate native code.

 * `--posix`:
   The generated makefile will work with any POSIX compliant make. Otherwise the
   output may use extensions specific to GNU make.

## RELAXING RULES

The following can be specified as a comma-separated list to the `--relax` option to
disable certain semantic rule checks.

* `prefer-explict`:
  Any visible explicitly declared operator always hides an implicit
  operator regardless of the region in which it is declared. This is required to
  analyse code that uses the Synopsys `std_logic_arith` package.

* `generic-static`:
  References to generics are allowed in locally static expressions using the
  VHDL-2008 rules.

* `universal-bound`:
  Prior to VHDL-2000 when range bounds have universal integer type the expressions
  must be either numeric literals or attributes. This option
  allows ranges such as `-1 to 1` in VHDL-1993 which otherwise must be written
  `integer'(-1) to 1`.

## SELECTING SIGNALS

Every signal object in the design has a unique hierarchical path name. This is
identical to the value of the `PATH_NAME` attribute. You can get a list of the
path names in your design using the command `show [signals]` from the TCL shell.

A signal can be referred to using its full path name, for example
`:top:sub:x`, and `:top:other:x` are two different signals. The character `:` is a
hierarchy separator. A _glob_ may be used refer to a group of signals. For example
`:top:*:x`, `*:x`, and `:top:sub:*`, all select both of the previous signals. The
special character `*` is a wildcard that matches zero or more characters.

### Restricting waveform dumps

Path names and globs can be used to exclude or explicitly include signals in a
waveform dump. For simple cases this can be done using the `--include` and
`--exclude` arguments. For example `--exclude=":top:sub:*"` will exclude all
matching signals from the waveform dump. Multiple inclusion and exclusion
patterns can be provided.

When the number of patterns becomes large, specifying them on the command line
is cumbersome. Instead a text file can be used to provide inclusion and
exclusion patterns. If the top-level unit name is `top` then inclusion patterns
should be placed in a file called `top.include` and exclusion patterns in a file
called `top.exclude`. These files should be in the working directory where the
`nvc -r` command is executed. The format is one glob per line, with comments
preceded by a `#` character.

When both inclusion and exclusion patterns are present, exclusions have precedence
over inclusions. If no inclusion patterns are present then all signals are
implicitly included.

## VHPI

NVC supports a subset of VHPI allowing access to signal values and events at
runtime. The standard VHPI header file `vhpi_user.h` will be placed in the system
include directory as part of the installation process. VHPI plugins should be
compiled as shared libraries; for example:

    $ cc -shared -fPIC my_plugin.c -o my_plugin.so
    $ nvc -r --load my_plugin.so my_tb

The plugin should define a global `vhpi_startup_routines` which is a NULL-terminated
list of functions to call when the plugin is loaded:

    void (*vhpi_startup_routines[])() = {
       startup_1,
       startup_2,
       NULL
    };

TODO: describe VHPI functions implemented

## LIBRARIES

Description of library search path, contents, etc.

## CODE COVERAGE

Description of coverage generation

## TCL SHELL

Describe interactive TCL shell

## AUTHOR

Written by Nick Gasson

## REPORTING BUGS

Report bugs using the GitHub issue tracker at<br>
<https://github.com/nickg/nvc/issues>
