## master
- Library build is now reproducible when running `make -j`.
- Fix a constant folding crash with nested records.
- The `--relax=impure` option allows pure functions to call impure
  functions.
- Name resolution and overload resolution has been completely rewritten
  which should fix a number of long-standing issues.
- Fixed a crash when a record aggregate contains an "others" association
  and the fields have array types with different lengths.
- Added support for VHDL-2008 "all" sensitised processes.
- Added support for ports and generics in block statements.
- Added support for the 'BASE attribute.
- Type name now allowed in element association choice (#407).
- Implement textio READ procedure for BIT, TIME, and REAL (#408).
- LLVM 6.0 or later required to build.
- Fixed a crash when a long running procedure suspends in a loop (#412).
- Fixed a stack overflow when a subprogram with unconstrained array
  arguments is called repeatedly in a loop (#414).
- Compiled VHDL code now includes DWARF debug information which is used
  for runtime stack trace if libdw or libdwarf is installed.
- Added support for MINIMUM, MAXIMUM, and TO_STRING predefined operators
  in VHDL-2008.
- Support for the LXT wave output format, which was deprecated in
  version 1.5, has been removed. Use the default FST format instead.
- VCD files are now generated from FST data in a similar manner to
  `fst2vcd(1)`. This should improve compatibility with other tools.
- The `fetch-ieee.sh` script which did nothing since the last release
  has been removed.
- Added support for VHDL-2008 element resolution.
- The VHDL-1993 standard libraries are now derived from the Apache 2.0
  licensed sources from VHDL-2019.
- Added support for VHDL-2008 reduction operators, match operators, and
  condition conversion.
- VHDL-2008 IEEE standard libraries are now built and installed in
  addition to the VHDL-1993 libraries.
- Variable assignment now supports aggregate targets.
- Fix a crash when using 'VALUE with enumeration subtypes (#419).
- The `--codegen` command, which has been deprecated since 1.3, was
  removed.
- The `--profile` option now prints internal simulation statistics
  instead of the top processes by CPU time.
- The elaboration phase was largely written which fixes a number of
  long-standing issues and significantly improves elaboration speed.
- Added support for 'LAST_ACTIVE attribute (#423).
- Added support for 'DRIVING and 'DRIVING_VALUE attributes.
- Fixed intermittent crash when evaluating nested constant records
  (#425).
- Fixed missing import libraries on Windows (#424).
- Standard libraries are now installed under `$prefix/lib/nvc/` instead
  of `$prefix/share/nvc`.
- Added a new option `--ieee-warnings=off` to disable warning messages
  from the standard IEEE packages.
- Support for configurations has been significantly improved (#372).
- Added support for VHDL-2008 delimited comments.

## 1.5 - 2020-07-19
- IEEE library sources are now distributed
- Updated FST library to match GtkWave 3.3.79
- The LXT wave output format is deprecated, use FST instead
- Fix incorrect file name in assertion message
- Fix crash while recovering from parse error
- Add `--dump-json` command to print AST as JSON (from Sebastien Van
  Cauwenberghe)
- Fix crash when using LLVM 7 and later
- Fix spurious assertion failure in `std.textio.readline`
- Reals are now rounded to the nearest integer as specified by the LRM
- Fix crash when constant folding uses too much memory
- Improved memory management in evaluator (thanks to Frank Mori Hess)
- Various other minor fixes and improvements

## 1.4 - 2018-07-16
- Windows with MSYS2 is now fully supported
- Continuous integration for OS X and Windows using Travis and AppVeyor
- Record resolution functions are now supported
- The `--relax=prefer-explicit` option now works correctly

## 1.3 - 2017-12-31
- Native code is always generated on platforms that support it
- Initial support for VHDL-2017 conditional compilation blocks
- The TCL shell has been removed
- Various code generation and constant folding fixes
- Fix crash when build IEEE library from non-clean tree
- ORC is used for JIT if LLVM version is 3.9 or later
- LLVM 3.8 or later is now required
- Added `-O0` to `-O3` options to control LLVM optimisation level
- The `--disable-opt` option is deprecated, use `-O0` instead
- The elaborate `--native` option is now a global option and should
  be placed before the `-e` command
- The `--codegen` command is deprecated, use `--native` when analysing
  to generate native shared libraries for packages

## 1.2 - 2017-05-04
- Compile time evaluation now uses vcode
- Record aggregates can now be constant folded (#305)
- The TCL shell is deprecated and will be removed in the next release
- Compatible with LLVM 4.0
- Functions returning records can now be constant folded (#315)
- LLVM 3.4 or later is now required

## 1.1 - 2016-11-05
- Improvements to based literal parsing
- Improved stack traces on Linux when `libdw-dev` installed
- Fixed elaboration of subprograms in entity declarations
- Improved elaboration performance when design contains many signals
- Fixed various bugs involving record signals
- Fixed sensitivity list generation for concurrent procedure calls
- Improvements to simulation performance
- Implemented `vhpi_get_phys`
- Added command `--list` to print all units in a library
- Rewrote the `run_regr` program in C so `make check` no longer requires Ruby
- The `--relax=generic-static` option is now renamed to `--relax=locally-static` and
  applies in wider range of contexts

## 1.0 - 2016-05-01
- First stable release
