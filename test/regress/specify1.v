module specify1;

    reg d = 1'b0;
    reg en = 1'b1;
    reg clk = 1'b0;

    sub i_sub(
        .d  (d),
        .en (en),
        .clk(clk)
    );

    assign #5 clk = ~clk;

    initial
    begin
        #4;
        d <= 1'b1;
        #4;
        d <= 1'b0;
        #8;
        d <= 1'b1;
        en <= 1'b0;
        #3;
        d <= 1'b0;
        #7;
        $finish;
    end

endmodule

module sub(
    input d,
    input en,
    input clk
);

  localparam p_dummy = 1;

  specify
    // Both should fail at time 5
    $setup(d, posedge clk, 2);
    $setup(d, posedge clk, 1.8);

    // Should fail at time 10 and 20
    $setup(d, negedge clk, 3);

    // Should fail at time 20
    $setup(d, negedge clk, 1.99);

    // Should not fail - Gated by cond
    $setup(d, negedge clk &&& en, 2);

    // Should fail at time 16
    $hold(clk, d, 2);
  endspecify

endmodule
