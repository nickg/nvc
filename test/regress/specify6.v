module specify6;

    reg d   = 1'b0;
    reg clk = 1'b0;

    sub i_sub(
        .d   (d),
        .clk (clk)
    );

    always #5 clk = ~clk;

    initial
    begin
        #4;
        d = 1'b1;
        #20;
        $finish;
    end

endmodule

module sub(
    input d,
    input clk
);

  specify
    // Check fires: clk edge 5, data edge 4, diff 1, limit 2
    $setup(d, posedge clk, 2);
  endspecify

endmodule