## 1.5.1 - TODO
- Fix a compiler warning in vcode.c.

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
