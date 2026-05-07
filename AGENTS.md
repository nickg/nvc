NVC is a mixed-language VHDL and SystemVerilog compiler and simulator.

The `build/` directory contains only compiled output and no source code.

To build the project run `make -C build`.

After changing node definitions, for example in `src/vlog/vlog-node.c`,
run `make -C build bootstrap` so serialized library formats are regenerated.

To run the unit tests use `make -C build check QUICK=1`.

Regression tests are listed in `test/regress/testlist.txt`.  New tests
should be appended at the end.  Run the regression runner from the build
directory to execute a single test:

    cd build
    bin/run_regr <test-name>

Equivalently, from the repository root:

    make -C build
    (cd build && bin/run_regr <test-name>)

To run the full test suite use `make -C build check`.  This can take a
long time so use it sparingly and prefer targeted tests.

Prefer writing tests that are self checking, either with asserts (VHDL)
or by printing PASSED/FAILED (Verilog).  If the output of the simulator
must be checked, place the expected output in `test/regress/gold` and
add the "gold" option to the entry in `testlist.txt`.  Avoid writing
shell script tests if possible.

There's no need to update `test/dist.mk`, this is generated
automatically before release.

Avoid running multiple tests or builds in parallel.

New regression tests should be appended to the end of
`test/regress/testlist.txt`.
