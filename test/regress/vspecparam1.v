module vspecparam1;
  reg d = 1'b0;
  reg clk = 1'b0;

  vspecparam1_sub u (.d(d), .clk(clk));

  assign #5 clk = ~clk;

  initial begin
    #4 d = 1'b1;
    #4 d = 1'b0;
    #10 $finish;
  end
endmodule

module vspecparam1_sub (input d, input clk);
  specify
    specparam tSU = 2, tSU_OK = 1;
    specparam tH = 0:4:6;      // Typical value is used
    specparam tR = 2.0;

    $setup(d, posedge clk, tSU);       // Fires at 5
    $setup(d, posedge clk, tSU_OK);    // Does not fire
    $hold(posedge clk, d, tH);         // Fires at 8
    $setup(d, posedge clk, tR + 1);    // Fires at 5
  endspecify
endmodule
