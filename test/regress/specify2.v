module specify2;

    reg d   = 1'b0;
    reg ena = 1'b1;
    reg clk = 1'b0;

    sub i_sub(
        .d   (d),
        .ena (ena),
        .clk (clk)
    );

    assign #5 clk = ~clk;

    initial
    begin
        #3;
        d <= 1'b1;
        #1;
        ena <= 1'b0;
        #4;
        d <= 1'b0;
        #6
        d <= 1'b1;
        #5
        d <= 1'b0;
        #2;
        $finish;
    end

endmodule

module sub(
    input d,
    input ena,
    input clk
);

  specify
    // Check fires (ev0 cond):
    //      clk edge:  5
    //      data edge: 3
    //      diff:      2
    //      limit      3
    // ena=1 at D posedge, but low at clk posedge
    // Should fail at time 5
    $setup(posedge d &&& ena, posedge clk, 3);

    // Check fires (ev1 cond - active low):
    //      clk edge:  10
    //      data edge: 8
    //      diff:      2
    //      limit      3
    // ena=0 at both at d and clk negedge
    // Should fail at time 10 and 20
    $setup(d, negedge clk &&& ~ena , 3);

    // Check does not fire (ev0 gated by cond):
    //      clk edge:  5
    //      data edge: 4
    //      diff:      1
    //      limit      2
    // Should fail at time 5
    $setup(d &&& ena, posedge clk, 2);

    // Check does not fire (ev1 gated by cond):
    //      clk edge:  5
    //      data edge: 4
    //      diff:      1
    //      limit      2
    // Should fail at time 5
    $setup(negedge d, negedge clk &&& ena, 2);

  endspecify

endmodule
