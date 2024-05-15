## Unreleased changes

## Version 1.12.2 - 2024-05-15
- Fixed a crash when `'transaction` is used with a record type.
- Fixed a compatibility issue with LLVM 18 that could cause an illegal
  instruction exception when the `--jit` elaboration option is used
  (#887).

## Version 1.12.1 - 2024-05-09
- Fixed a crash when a process contains a `force` or `release`
  assignment inside a `process (all)` statement (#877).
- Fixed a crash creating a waveform dump with `--dump-arrays` and the
  design contains an array-of-array-of-records (#878).
- Fixed a spurious error when a type declaration from a package generic
  is used in a port list (#881).
- Fixed an assertion failure when a procedure declared within a `process
  (all)` assigns to a signal (#882).
- Added a missing check for illegal assignment to external signal name
  outside a process (#884).
- Fixed a crash while printing diagnostic information when the
  simulation delta cycle limit is reached (#885).
- Generic actuals no longer need to be globally static, which was never
  required by the LRM (#886).

## Version 1.12.0 - 2024-04-07
- The `--jit` elaboration option no longer requires `--no-save`.
- Fixed a crash when subtype bounds depend on a package instance generic
  (#815).
- Fixed various issues in the implementation of guarded blocks and
  disconnection specifications (#829).
- The `--std` option now controls which VHDL standard the `--install`
  command uses to compile third-party libraries (#836).
- The prefix of an indexed or slice name now must be another name or
  function call, as required by the LRM (#835).
- The implementation of conversions in port maps has been reworked and
  fixes a number of long-standing issues (#843).
- Added support for `inertial` keyword in port maps (#843).
- Fixed a bug where `'last_value` could give the wrong result if the
  signal has multiple sources.
- Updated to OSVVM 2023.09a and UVVM 2023.09.16 for `nvc --install`.
- The `--exit-severity=` option now also controls which severity level
  results in a non-zero exit code (#850).
- Improvements to waveform dumping for signals with record types or
  types with non-locally-static bounds (#851, #852).
- The parameter for attributes with dimensions such as `'length(N)` may
  be any integer type when `--relaxed` is passed (#862).
- Resolved several other minor issues (#654, #854, #855, #859, #863).
- The default standard version was changed to VHDL-2008.
- The `--vhpi-trace` option now implies `--vhpi-debug`.
- The bounds of array aggregates are now calculated correctly in several
  corner cases (#873).
- Added supported for VHPI foreign subprograms.

## Version 1.11.3 - 2024-02-04
- Fixed incorrect effective value when a signal has multiple sources due
  to port-collapsing optimisation (#824).
- Signals that appear in aggregate choice expressions like `(S downto 0
  => '0')` are now included in the sensitivity list for concurrent
  statement equivalent processes, and in `process (all)` (#825).
- The direction of aggregates with range choices is now calculated
  correctly in VHDL-2008 mode (#826).
- Fixed a memory corruption bug when evaluating certain aggregates that
  contain a range association (#827).
- Resolved several other minor issues (#837, #839, #840).

## Version 1.11.2 - 2024-01-04
- Fixed an incorrect length check in the equivalent process for
  non-static port map actuals (#817).
- Library file names for design units that contain extended identifiers
  such as `/Foo/` are now encoded in a way that avoids illegal
  characters and case sensitivity issues on Windows and macOS (#816).
- Implemented `vhpiIsNullP` and `vhpiIsDiscreteP` for ranges.
- Fixed a crash when an aliased enumeration literal appears in a case
  choice expression (#819).
- Fixed calculation of longest static prefix with indexes and slices of
  a constant array (#820).
- Fixed a crash during elaboration when simplifying an if-statement that
  depends on a generate parameter (#821).
- Increased the limit on the number of subprogram arguments to allow
  compiling some Lattice IPs (from @sean-anderson-seco).

## Version 1.11.1 - 2023-12-16
- Fix crash during elaboration when an if-statement branch is always
  taken (#812).
- VHPI plugins can now access types in instantiated packages (from
  @sean-anderson-seco).
- The `--version` output now includes the bare Git commit SHA if the
  program is built from a shallow clone with no tags (#813).

## Version 1.11.0 - 2023-12-06
- New command `--cover-export` exports coverage data in the Cobertura
  XML format which is supported by most CI environments such as GitLab.
- Generics on internal instances can now be overridden with the `-g`
  elaboration option.  For example `-g uut.value=42`.
- Implemented the `'reflect` attribute and associated protected types
  from VHDL-2019.
- Added support for VHDL-2019 sequential block statements.
- Implemented the VHDL-2019 directory I/O functions in `std.env`.
- Added VHDL-2019 assert API (with @Blebowski).
- Implemented `'image`, `'value` and `to_string` for composite types in
  VHDL-2019.
- Implemented the "closely related record types" feature from VHDL-2019.
- Implemented the "composition with protected types" feature from
  VHDL-2019.
- The new `--shuffle` option runs processes in a random order which can
  help to identify code that depends on a particular execution order.
- Updated to OSVVM 2023.07 for `nvc --install`.
- Various enhancements and fixes to the VHPI implementation.
- Implemented the VHDL-2019 changes to `instance_name` and `path_name`
  for protected type variables.
- VHPI error messages are no longer reported as diagnostic messages on
  the console.  The new `--vhpi-debug` option restores the old
  behaviour.
- Support for type conversions between arrays with closely related
  element types.
- Added support for FSM state coverage collection (from @Blebowski).
- An alias of a type now correctly creates implicit aliases for each
  predefined operator of that type (#776).
- Improve overload resolution where a partial named association implies
  the formal parameter must be an array (#793).
- Handling of implicit conversion for universal types has been reworked
  to better comply with the LRM.
- Fixed a crash when string literal characters have a type which is an
  alias to another type (#801).
- Added a warning when calling the predefined `"="` and `"/="` operators
  on arrays and the left and right hand sides have different lengths.
- Expressions like `abs(x)**2.0` are now parsed correctly (#805).

## Version 1.10.4 - 2023-10-16
- Fixed compatibility with LLVM 17.
- Fixed an intermittent crash when using the `--jit` elaboration option.
- Improved overload resolution when subprogram argument is known to have
  character type.
- Improved bounds checking for port map actuals.
- Fixed a crash with aggregates for record types that contain an
  unconstrained field (#768).
- Fix spurious "checksum 00000000" error when analysing multiple design
  units and one contains `use work.all` (#769).
- Fix crash when using the `--relaxed` analysis option and multiple
  warnings are printed (#770).

## Version 1.10.3 - 2023-09-17
- Fixed memory corruption in rare circumstances with functions declared
  in process declarative regions (#751).
- Fixed a race condition when creating library directories that
  occasionally led to build failures with `make -j` (#746).
- Aliases of protected type methods are now allowed.
- Fixed a crash when a pragma appears outside of a design unit (#752).
- Fixed a crash analysing an alias of an alias of a subprogram (#755).
- Fixed an elaboration failure when an allocator expression has an
  array type with an unconstrained element (#756).
- Fixed a memory leak in the `--print-deps` command.
- Fixed a crash when evaluating globally static expressions during
  elaboration with coverage enabled (#759).
- Fixed an analysis crash where a predefined function for a type
  declared in a package is overridden in the package body only (#760).

## Version 1.10.2 - 2023-08-20
- Fixed a crash due to an array bounds check being incorrectly optimised
  out (#747).
- The type of string literals and aggregates in generic map associations
  such as `G => X"00"` where `G` has a generic type can now be
  determined correctly (#750).
- Fixed a spurious "duplicate declaration" error if a use clause for an
  instantiated packages appears in the same declarative region that
  instantiated the package (#750).
- Improved checking for configuration specifications.
- Empty bit string literals are now parsed correctly.
- Fixed stack corruption when a function returns an alias of one of its
  array arguments.

## Version 1.10.1 - 2023-07-28
- Fixed incorrect sensitivity list generation with concurrent statements
  such as `x <= C(y)` where C is a constant array.
- Fixed an incorrect optimisation which caused `'event` to return the
  wrong value in rare circumstances.
- Decimal bit string literals more than 64 bits wide are now supported
  (#731).
- The format of fractional `time` values returned by the standard
  `to_string` function was changed to match other simulators.
- Fixed a crash when constant folding a locally static expression in a
  package body (#742).
- Added support for reading command line arguments in VHPI (from
  @Forty-Bot).
- Fixed a compilation error when using the predefined
  `minimum`/`maximum` functions with arrays of physical types.
- Overloaded protected procedure calls could read the wrong value of
  protected type variables in rare circumstances.

## Version 1.10.0 - 2023-07-14
- The Zstandard compression library is now a build dependency.  Install
  `libzstd-dev` or similar.
- The `integer` type is now 64-bit in VHDL-2019 mode.
- The [VUnit](https://vunit.github.io/) VHDL libraries can now be
  installed with `nvc --install vunit` but please note this does not
  install the Python infrastructure.
- Updated to OSVVM 2023.05 and UVVM 2023.03.21 for `nvc --install`.
- Conditional expressions are now allowed in constant, signal, and
  variable declarations in VHDL-2019 mode.
- Conditional return statements are now supported in VHDL-2019.
- Added support for the "function knows vector size" feature in
  VHDL-2019.
- Entity ports with variable class and protected type are now supported
  in VHDL-2019 mode.
- The [xpm_vhdl](https://github.com/fransschreuder/xpm_vhdl) project
  which provides VHDL models of the Xilinx XPM macros can now be
  installed with `nvc --install xpm_vhdl`.
- Many improvements to the VHPI implementation (from @Forty-Bot).
- `vhpi_put_value` with `vhpiDepositPropagate` mode is now supported.
- The Synopsys `std_logic_misc` package is now compiled for 2008 (#696).
- Fixed an issue where leading `NUL` characters in a report message
  would prevent the entire message being printed (#700).
- Added support for interfaces in VHDL-2019 including mode view
  declarations, mode view indications, and the `'converse` attribute.
- Added support for VHDL-2008 matching `select?` statements (#705).
- Added support for the new `'designated_subtype` and `'index`
  attributes in VHDL-2019.
- Implemented the date/time functions from `std.env` in VHDL-2019.
- The default exit severity was changed from `error` to `failure`.  This
  means a failing assertion no longer immediately terminates the
  simulation.  The old behaviour can be restored with
  `--exit-severity=error`.
- Comparison operators as well as `minimum`/`maximum` functions are now
  defined for all scalar array types in VHDL-2019.
- Added support for selected signal and variable sequential assignment
  statements.
- The `-a` analysis command now accepts an `-f list` option where `list`
  is a text file containing a list of files to analyse.  Alternatively
  this may be written `@list`.
- Accesses to protected types and files are now allowed in VHDL-2019.
- Fixed a crash when indexing a null array (#734).
- Named and range choices are now supported in aggregate targets of
  variable and signal assignments (#712).
- The `synopsys.attributes` package is no longer distributed or built as
  part of the standard libraries.

## Version 1.9.2 - 2023-05-01
- Fix elaboration errors with recursive entity instantiation (#668).
- Updated to latest GtkWave FST writer library.
- Fix crash when an external name is used in a wait expression (#674).
- Fix stack corruption when passing large numbers of arguments (#665).
- Protected procedure calls with `out` or `inout` signal arguments now
  create drivers (#675).
- Fixed a crash when a `next` or `exit` statement appears inside
  `process (all)` (#676).
- The `--jit` elaboration options now works on Intel Macs (#680).
- Fixed a corner case where a forced signal with no drivers had the
  wrong effective value after `release` (#681).
- Fixed a crash when a signal assignment statement contains only
  `unaffected` (#677).

## Version 1.9.1 - 2023-04-15
- Fix build errors and warnings on MSYS2 Clang x64 environment.
- Fix build failure due to missing `SHT_X86_64_UNWIND` on Alpine and
  Ubuntu 18.04 (#666).
- Elaboration now works correctly when the `--with-system-cc=` configure
  argument is given a non-absolute path (#667).
- Fixed a crash when a subprogram declared in a for-generate block is
  called during elaboration (#668).
- Real to physical or 64-bit integer conversion now produces the correct
  result when evaluated during elaboration (#669).
- Matching `case?` statements are now handled correctly in inside
  `process (all)` (#670).
- Pass `-no_fixup_chains` to linker on macOS to avoid warning.
- `;` can now be used as a separator in the `--work` and `--map`
  arguments on Windows in addition to `:` (#671).

## Version 1.9.0 - 2023-04-07
- Code generation has been rewritten to enable faster elaboration and
  "just-in-time" compilation in the future.
- Now compatible with LLVM 16.
- Implemented the VHDL-2019 call path reporting API.
- The `elsif` VHDL-2019 conditional analysis directive now works
  correctly (#604).
- The `'transaction` implicit signal no longer incurs a delta-cycle
  delay.
- `x'ascending` now reports the correct result if `x` has unconstrained
  array type and null range.
- The predefined `"="` operator on record types now always uses the
  predefined equality comparison for fields even in the presence of a
  user-defined `"="` operator.
- It is no longer necessary on Windows to link VHPI plugins at
  elaboration time with `NVC_FOREIGN_OBJ`.  Use the `--load` option to
  load the plugin at run time as on other operating systems.
- The experimental `--jit` elaboration option defers native code
  generation until run time.  This can dramatically reduce total test
  time for short-running simulations.
- Statements like `wait for X` where `X` is negative but not a constant
  now produce an error at run time (#633).
- NVC is now supported by [VUnit](https://vunit.github.io/).
- Implicit signal attributes like `'transaction` are now considered
  static signal names (#640).
- Added support for fine-grained coverage collection via
  `--coverage-spec` elaboration option (from @Blebowski).
- The ABI for passing unconstrained arrays to foreign subprograms
  changed slightly, see the manual for details.
- Implemented new file I/O operations from VHDL-2019.
- Added analysis option `--define` to set user-defined conditional
  analysis identifiers (from @Blebowski).
- Optional support for using ZSTD to compress library files if
  `libzstd-dev` is installed.
- ISO-8859-1 extended characters are now handled properly in identifiers
  and when printing to the terminal.
- The new `configure` option `--disable-default-paths` disables the
  default library search paths (#652).
- Subtype indications used as case range choices no longer crash during
  analysis (#655).
- The default standard version was changed to VHDL-2002 and will likely
  change again to -2008 in a future release.  Users are recommended to
  use the `--std=` option to specify an explicit standard revision to
  avoid any compatibility issues.
- Fixed a crash when elaborating a port map which contains a subtype of
  a record (#662).
- Implemented VHDL-2019 syntax relaxations for empty records and
  trailing semicolon in interface lists (from @bpadalino).
- A Bash auto-completion script is now installed by default.  Run
  `configure` with `--without-bash-completion` to disable this.

## Version 1.8.2 - 2023-02-14
- Fixed "failed to suspend thread" crash on macOS.
- Fix incorrect coverage scope nesting with array case statements (from
  @Blebowski).
- Expressions like `FOO(X)'DELAYED` now work as expected rather than
  producing a fatal error (#603).
- Fixed a bug where data was not propagated through inout ports in
  certain conditions (#609).
- The `-gNAME=VALUE` option to set generic values from the command line
  now works correctly for subtypes of enumeration types (#618).
- Fixed crash when creating an array of record subtypes where the
  subtype declaration has an element constraint (#615).

## Version 1.8.1 - 2023-01-23
- Initial signal values for certain types were not dumped correctly in
  FST files on Windows (#596).
- Missing `thirdparty/str-two-way.h` from distribution tarball (#599).

## Version 1.8.0 - 2023-01-22
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
- The `--make` command is deprecated and will be repurposed in a later
  release.  Use the new `--print-deps` command instead to generate
  Makefile dependencies.

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
