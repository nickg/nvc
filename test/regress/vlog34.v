module vlog34;
  wire [7:0] x;
  reg        reset = 1;
  reg [7:0]  y = 0;

  assign x = reset ? 0 : x + 1;

  initial begin
    #1;
    reset = 0;
  end

endmodule // vlog33
