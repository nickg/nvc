nvc(1) -- VHDL Compiler and Simulator
=====================================

## SYNOPSIS

`nvc` -a [_options_] _files_...<br>
`nvc` -e [_options_] _unit_<br>
`nvc` -r [_options_] _unit_<br>

## DESCRIPTION

NVC is an implementation of the VHDL language as defined by IEEE standard 1076-1993
and later revisions. Simulating a design typically involes three steps: `analysing`
one more more source files into the work library; `elaborating` a top-level design
unit; and `running` the elaborated design.

## OPTIONS

NVC accepts three kinds of options: global options; a single command option; and
options specific to the command. Global options must be placed before the command and
specific options must be placed after the command.

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

### Global options

 * `-h`, `--help`:
   Display usage summary.

 * `-L` _path_:
   Add _path_ to the list of directories to search for libraries (see
   [LIBRARIES][] section below).

 * `-v`, `--version`:
   Display version and copyright information.

 * `--work=` _name_:
   Use _name_ as the work library (see [LIBRARIES][] section below).

### Analysis options

 * `--bootstrap`:
  Allow compilation of the STANDARD package. Not intended for end users.

 * `--prefer-explicit`:
  Any visible explicitly declared operator always hides an implicit operator
  regardless of the region in which it is declared. This deviates from the
  VHDL standard but is required to analyse code that uses the Synopsys
  `std_logic_arith` package.

### Elaboration options

 * `--cover`:
   Enable code coverage reporting (see the [CODE COVERAGE][] section below).

 * `--disable-opt`:
   Disable LLVM optimisations. Not generally useful unless debugging the
   generated LLVM IR.

 * `--dump-llvm`:
   Print generated LLVM IR prior to optimisation.

 * `--native`:
   Generate native code shared library. By default NVC will use LLVM JIT
   compilation to generate machine code at runtime. For large designs
   compiling to native code at elaboration time may improve performance.

### Runtime options

 * `-b`, `--batch`:
   Run in batch mode. This is the default.

 * `-c`, `--command`:
   Run in interactive TCL command line mode. See [TCL SHELL][] section below.

 * `--format=`_fmt_:
   Generate waveform data in format _fmt_. Currently supported formats are:
   `fst`, `lxt`, and `vcd`. The FST and LXT formats are native to GtkWave.
   The FST format is preferred over LXT due its smaller size and better
   performance; however VHDL support in FST requires a recent version of
   GtkWave so LXT is provided for compatibility. VCD is a very widely used
   format but has limited ability to represent VHDL types and the performance
   is poor: select this only if you must use the output with a tool that does
   not support FST or LXT. The default format is FST if this option is not
   provided.

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
   
 * `-w, --wave=`_file_:
   Write waveform data to _file_. The file name is optional and if not specified
   will default to the name of the top-level unit with the appropriate extension
   for the waveform format. The waveform format can be specified with the
   `--format` option.

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
