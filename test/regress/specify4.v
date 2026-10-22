module specify4;

    reg d   = 1'b0;
    reg clk = 1'b0;

    sub i_sub(
        .d   (d),
        .clk (clk)
    );

    initial
    begin
        #4;
        d = 1'b1;
        #1;
        clk = 1'b1;
        #0;
        d = 1'b0;
        #5;
        clk = 1'b0;
        #1
        d = 1'b1;
        #4;
        d = 1'b0;
        #0;
        clk = 1'b1;
        #5;
        $finish;
    end

endmodule

module sub(
    input d,
    input clk
);

  specify
    // Checks specific corner-case(s):
    //             ____
    //  D:    _____|  |___________
    //                ____________
    //  CLK:  ________|
    //
    // If D violates t-check before the CLK, and also at the same time
    // as CLK (in earlier or later delta), tcheck fail exactly once
    // (not forget, not fire twice)!
    //
    // If D changes only the same cycle as CLK, tcheck shall not fire!
    //                _____________
    //  D:    ________|
    //                ____________
    //  CLK:  ________|
    //
    // Should fail at 5 only!
    $setup(d, posedge clk, 2);

    // Should fail at 5 and 15 -> Simultaneous D and CLK!
    $hold(negedge d, posedge clk, 2);

  endspecify

endmodule
