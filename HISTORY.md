## master
- The TCL shell has been removed
- Various code generation and constant folding fixes
- Fix crash when build IEEE library from non-clean tree
- ORC is used for JIT if LLVM version is 3.9 or later

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
