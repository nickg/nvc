module specify1;

    reg d   = 1'b0;
    reg clk = 1'b0;

    sub i_sub(
        .d   (d),
        .clk (clk)
    );

    assign #5 clk = ~clk;

    initial
    begin
        #4;
        d <= 1'b1;
        #4;
        d <= 1'b0;
        #10;
        $finish;
    end

endmodule

module sub(
    input d,
    input clk
);

  specify
    // Check fires (integer limit):
    //      clk edge:  5
    //      data edge: 4
    //      diff:      1
    //      limit      2
    // Should fail at time 5
    $setup(d, posedge clk, 2);

    // Check fires (real limit):
    //      clk edge:  5
    //      data edge: 4
    //      diff:      1
    //      limit      1.8
    // Should fail at time 5
    $setup(d, posedge clk, 1.8);

    // Check does not fire (setup with limit = diff):
    //      clk edge:  5
    //      data edge: 4
    //      diff:      1
    //      limit      1
    // Should NOT fail
    $setup(d, posedge clk, 1);

    // Check fires (negative ev1):
    //      clk edge:  10
    //      data edge:  8
    //      diff:       2
    //      limit:      3
    // Should fail at time 10
    $setup(d, negedge clk, 3);

    // Check does not fire (negative ev1, positive ev0):
    //      clk edge:  10
    //      data edge:  4
    //      diff:       6
    //      limit:      5
    // Should not fail
    $setup(posedge d, negedge clk, 5);

    // Check fires (negative ev1, positive ev0):
    //      clk edge:  10
    //      data edge:  4
    //      diff:       6
    //      limit:      7
    // Should fail at time 10
    $setup(posedge d, negedge clk, 7);

  endspecify

endmodule
