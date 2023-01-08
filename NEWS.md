## Unreleased changes
- The `--disable-opt` and `--native` elaborate options which were
  deprecated in version 1.3 have been removed.
- The JSON dumper which was unmaintained for several years has been
  removed.
- VHDL-2019 mode can be enabled with `--std=2019`.  Please note there is
  very limited support for this standard at present.
- The new `--no-save` elaboration option skips saving the elaborated
  design and other generated files to the working library.
- Added support for `else` and `elsif` in generate statements (#510).
- Xilinx Vivado vendor libraries can now be compiled with `nvc --install
  vivado`.
- LLVM 8.0 or later is now required due to deprecation of non-opaque
  pointers.
- Altera/Intel Quartus vendor libraries can now be compiled with `nvc
  --install quartus`.
- The `nvc --version` output now includes the commit hash if built from
  a Git checkout.
- The new `--gtkw` run option writes a `.gtkw` save file for GtkWave
  containing all the signals in the design (suggested by @amb5l).
- `libffi` is now a build-time dependency.
- Negation of the smallest negative value of a type such as
  `-integer'left` now produces an error.
- Default OSVVM version updated to 2022.11.
- `case .. generate` statements are now supported in VHDL-2008.
- Coverage implementation was reworked and now collects statement,
  branch, expression and toggle metrics (from @Blebowski).

## Version 1.7.2 - 2022-10-16
- Fixed build on FreeBSD/arm (#534).
- Fixed crash with generic package instantiation (#520).
- Now compatible with LLVM 15.0.
- Fixed calculation of longest static prefix with `'RANGE` expression
  (#542).
- `MOD` and `REM` are now defined for physical types in VHDL-2008
  (#540).
- Signal declarations are now allowed in entity declarative part (#547).
- Protected objects are now allowed in concurrent procedure calls
  (#547).
- Opening a file with `APPEND_MODE` now actually appends to the file
  instead of truncating (#551).

## Version 1.7.1 - 2022-08-31
- Added missing textio `WRITE [LINE, REAL, STRING]` in VHDL-2008.
- Added support for FreeBSD/powerpc (#503, #504, from @pkubaj).
- Fixed "missing vcode unit" error during elaboration (#502).
- Fixed crash with recursive entity instantiation (#502).
- Fixed error with expressions like `X'ELEMENT'LENGTH` (#508).
- Added support for FreeBSD/i386.
- Fixed crash reading resolved value of record signal (#502).
- Improved folding of for-generate expressions (#514).
- Fixed memory leak when forcing signals.
- Fixed crash with type conversion in generic map (#518).
- Fixed crash with expressions like `X'DELAYED'STABLE` (#517).
- External names now work with record signals (#520).
- Xilinx Vivado vendor libraries can now be compiled with `nvc --install
  vivado`.
- VITAL libraries are now built by default on Windows.
- Fixed build with old versions of Glibc.
- Resolution functions with nested record types now behave correctly
  (#516).
- Avoid repeated `stat(2)` calls when accessing library.
- Fixed very slow elaboration in some cases with function calls in
  generic map expressions.
- Fixed intermittent segfault during elaboration (#506).
- Fixed incorrect constant folding of case statements with range
  choices.
- Fixed crash with constrained array declared in entity.
- Real valued signals can now be dumped in FST files (#524).
- Fixed signal assignment delay with side effects being evaluated twice
  (#527).
- An error is now reported for duplicate choices in array-type case
  statements (#528).

## Version 1.7.0 - 2022-08-07
- *Breaking change:* In-tree builds are no longer supported: use a
  separate build directory instead.
- *Breaking change:* The `--force-init` command is deprecated and has no
  effect.
- Added support for VHDL-2008 type generics on packages and entities.
- Diagnostic messages have been enhanced with more contextual
  information.
- Added support for record element constraints and record fields with
  unconstrained array types.
- Alias of multidimensional array allowed in VHDL-2008 mode.
- Implemented VHDL-2008 rules for aggregates with slices.
- VHPI is now always enabled at build time and the `--enable-vhpi`
  configure option has no effect.
- Arithmetic operations that overflow the underlying machine type now
  produce an error (#101).
- Added support for VHDL-2008 force/release assignments.
- Basic support for external names in VHDL-2008.
- Matching case `case?` statements are supported in VHDL-2008 mode.
- Fixed several bugs in the implementation of guarded signals.
- Implemented VHDL-2008 rules for generic visibility.
- Shared variable declaration permitted in entity declaration.
- Case expression no longer requires a locally static subtype in
  VHDL-2008 mode (#460).
- The VHDL heap is now garbage collected as required by VHDL-2019 and
  the `deallocate` operator has no effect other than setting the access
  to `null`.
- A new global option `-H` specifies the size of the simulation heap and
  defaults to 16 megabytes.
- Concurrent procedure call allowed in entity statement part.
- Added support for `'SUBTYPE` and `'ELEMENT` attributes in VHDL-2008.
- The new top-level `--init` command creates a new empty library
  directory.
- The `-a` analysis command now reads from the standard input if the
  file name is `-`.
- Added support for array element constraints in VHDL-2008.
- The `--prefer-explicit` analysis option which was deprecated before
  the 1.0 release has been removed.
- A new `--relaxed` analysis option enables "relaxed rules" mode.  This
  has the same effect as enabling all the existing `--relax=` options.
  However some constructs will still produce warnings.
- The `--relax=` analysis option is deprecated and is now equivalent to
  passing `--relaxed`.  The individual options are ignored.
- Added support for generic subprograms in VHDL-2008.
- New command `--install` allows easy installation of common third-party
  packages such as OSVVM and UVVM.
- Identifiers in waveform dumps are now in lower case instead of upper
  case.
- The function `CURRENT_DELTA_CYCLE` in `NVC.SIM_PKG` can be used to
  query the current delta cycle number.

## Version 1.6.2 - 2022-04-03
- Fix `make -j` with GNU make (#440).
- Subtraction from zero could return the wrong result in some
  circumstances.
- Fix incorrect code generation for access-to-array.
- Fix assertion failure with nested context.
- Generic declarations are now allowed to hide the entity name.
- Fix spurious error about duplicate declarations when using VHDL-2008
  context declarations.
- Fix incorrect default generic value when component and entity specify
  generics in different order (#448).
- Postponed processes with sensitivity lists now work correctly.
- Unit tests no longer fail when built without debug symbols (#445).
- Simulation now correctly terminates when time reaches `TIME'HIGH`.
- Assertion failure and report messages were not printed during the
  initialisation phase of the simulation.

## Version 1.6.1 - 2022-02-05
- Fix compatibility with BSD make (#440).
- Fix an out-of-memory condition when initialising processes (#441).
- Fix incorrect generic value in binding specification (#442).
- Fix spurious error in overload resolution if required arguments follow
  optional arguments (#443).
- Fix intermittent crash generating code for enumerated types (#444).
- Minor elaboration performance optimisation.

## Version 1.6.0 - 2022-01-25
- Name resolution and overload resolution has been completely rewritten
  which should fix a number of long-standing issues.
- The elaboration phase was largely written which fixes a number of
  long-standing issues and significantly improves elaboration speed.
- VHDL-2008 IEEE standard libraries are now built and installed in
  addition to the VHDL-1993 libraries.
- The VHDL-1993 standard libraries are now derived from the Apache 2.0
  licensed sources from VHDL-2019.
- There is now a project website at
  [https://www.nickg.me.uk/nvc/](https://www.nickg.me.uk/nvc/). Please
  link to this in preference to the GitHub project page.
- Compiled VHDL code now includes DWARF debug information which is used
  for runtime stack trace if libdw or libdwarf is installed.
- Added support for VHDL-2008 reduction operators, match operators, and
  condition conversion.
- Added support for VHDL-2008 element resolution.
- Variable assignment now supports aggregate targets.
- The `--relax=impure` option allows pure functions to call impure
  functions.
- Added support for VHDL-2008 "all" sensitised processes.
- Added support for ports and generics in block statements.
- Added support for the 'BASE attribute.
- Type name now allowed in element association choice (#407).
- Implement textio READ procedure for REAL.
- LLVM 6.0 or later is now required to build.
- Added support for MINIMUM, MAXIMUM, and TO_STRING predefined operators
  in VHDL-2008.
- VCD files are now generated from FST data in a similar manner to
  `fst2vcd(1)`. This should improve compatibility with other tools.
- Added support for 'LAST_ACTIVE attribute (#423).
- Added support for 'DRIVING and 'DRIVING_VALUE attributes.
- Added a new option `--ieee-warnings=off` to disable warning messages
  from the standard IEEE packages.
- Support for configurations has been significantly improved (#372).
- Added support for VHDL-2008 delimited comments.
- Added support for guard expressions on blocks.
- Added support for guarded signals.
- Added support for HREAD, HWRITE, and other TEXTIO additions in
  VHDL-2008.
- Code generation now happens in parallel when LLVM is built with
  multi-threading enabled.
- Link time optimisation (LTO) is now enabled for release builds where
  supported.
- The default assertion failure message for certain simple scalar
  comparisons now shows the values of the left and right hand sides.
- Added support for VHDL-2008 conditional variable assignment
  statements.
- Added support for VHDL-2008 extended bit string literals.
- Non-globally-static actuals allowed in port maps in VHDL-2008 mode.
- Added support for VHDL-2008 sequential conditional signal assignment
  statements.
- Added basic support for package generics and package instantiation.
- Nested arrays can now be included in the waveform dump but only if the
  `--dump-arrays` option is passed. This is disabled my default due the
  significant performance and memory overhead.
- Added support for record types in waveform dump (#216).
- Added support for foreign subprograms using the VHPIDIRECT protocol.
- Library build is now reproducible when running `make -j`.
- Fix a constant folding crash with nested records.
- Fixed a crash when a record aggregate contains an "others" association
  and the fields have array types with different lengths.
- Fixed a stack overflow when a subprogram with unconstrained array
  arguments is called repeatedly in a loop (#414).
- Fixed intermittent crash when evaluating nested constant records
  (#425).
- Fixed missing import libraries on Windows (#424).
- Standard libraries are now installed under `$prefix/lib/nvc/` instead
  of `$prefix/share/nvc`.
- New configure option `--disable-vital` disables building the VITAL
  packages whose license status is unclear.
- Support for the LXT wave output format, which was deprecated in
  version 1.5, has been removed. Use the default FST format instead.
- The `fetch-ieee.sh` script which did nothing since the last release
  has been removed.
- The `--codegen` command, which has been deprecated since 1.3, was
  removed.
- The `--profile` option now prints internal simulation statistics
  instead of the top processes by CPU time.

## Version 1.5.3 - 2021-11-13
- Handle access(2) returning EPERM in macOS sandbox (#421).
- Fix race when multiple processes concurrently update a library.
- Fix `--syntax` command when file contains multiple design units.
- Allow constant folding of nand/nor/xor/xnor.
- Fix potential out of memory condition when evaluating complex assert
  expressions.
- Fix incorrect result of `mod` operator with negative operands.
- Fixed intermittent crash when evaluating nested constant records
  (#425).
- Buffer too small for printing TIME'HIGH (#98).

## Version 1.5.2 - 2021-07-28
- Link libexecinfo on FreeBSD.
- Implement textio READ procedure for BIT and TIME (#408).
- Fixed a crash when a long running procedure suspends in a loop (#412).
- Fix static linking with LLVM 12.0.
- Fix crash when assigning to a signal declared in a package.
- Fix incorrect recording of dependencies which caused a failure to load
  generated DLLs on Windows.
- Fix file locking error when a library is located on NFS (#417).
- Optimise loading large library index from disk.
- Fix a crash when using 'VALUE with enumeration subtypes (#419).
- Fix a crash when a signal with more than 256 elements is declared in a
  package (#420).

## Version 1.5.1 - 2021-04-09
- Fix a compiler warning in vcode.c.
- Disable VHDL backtrace on non-Linux systems as the symbol names cannot
  be parsed reliably (#385).
- Update to latest `pc_from_uncontext.m4` for Apple M1 support.
- Fix incorrect application of LRM rules for building an equivalent wait
  statement for concurrent statements.
- Library build is now reproducible when running `make -j` (#409).
- Fix assertion failure with nested record type (#404).
- Use Pandoc to generate the manual page.

## Version 1.5 - 2020-07-19
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

## Version 1.4 - 2018-07-16
- Windows with MSYS2 is now fully supported
- Continuous integration for OS X and Windows using Travis and AppVeyor
- Record resolution functions are now supported
- The `--relax=prefer-explicit` option now works correctly

## Version 1.3 - 2017-12-31
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

## Version 1.2 - 2017-05-04
- Compile time evaluation now uses vcode
- Record aggregates can now be constant folded (#305)
- The TCL shell is deprecated and will be removed in the next release
- Compatible with LLVM 4.0
- Functions returning records can now be constant folded (#315)
- LLVM 3.4 or later is now required

## Version 1.1 - 2016-11-05
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

## Version 1.0 - 2016-05-01
- First stable release
