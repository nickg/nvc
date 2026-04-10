// Test $info, $warning, $error (IEEE 1800-2017 §20.11).
// Non-terminating severity tasks with various format patterns.

module vlog45;
    integer x = 42;
    reg [7:0] bval = 8'hAB;

    initial begin
        // No arguments
        $info;
        $warning;
        $error;

        // Plain format strings
        $info("info plain");
        $warning("warning plain");
        $error("error plain");

        // %0d — minimum width decimal
        $info("x=%0d", x);

        // %d — default width decimal
        $info("x=%d", x);

        // %4d — explicit width decimal
        $info("x=%4d", x);

        // %0t — minimum width time
        $info("t=%0t", $time);

        // %h / %x — hex
        $info("hex=%0h", bval);
        $info("hex=%x", bval);

        // %b — binary
        $info("bin=%b", bval);

        // %s — string
        $info("str=%s", "hello");

        // %% — literal percent
        $info("100%%");

        $display("PASSED");
        $finish;
    end
endmodule
