module specify3;

    reg d   = 1'b0;
    reg clk = 1'b0;

    sub i_sub(
        .d   (d),
        .clk (clk)
    );

    assign #5 clk = ~clk;

    initial
    begin
        #6;
        d <= 1'b1;
        #6;
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

    // Check fires :
    //      clk edge:  5
    //      data edge: 7
    //      diff:      1
    //      limit      2
    // Should fail at time 5
    $hold(posedge clk, posedge d, 2);

    // Check fires :
    //      clk edge:  5
    //      data edge: 12
    //      diff:      7
    //      limit      8
    // Should fail at time 12
    $hold(posedge clk, negedge d, 8);

    // Check does not fire (limit = diff):
    //      clk edge:  10
    //      data edge: 12
    //      diff:      2
    //      limit      2
    // Should not fail
    $hold(negedge clk, negedge d, 2);

    // Check fires
    //      clk edge:  10
    //      data edge: 6
    //      diff:      4
    //      limit      5
    // Should fail at time 10
    $hold(posedge d, negedge clk, 5);

  endspecify

endmodule
