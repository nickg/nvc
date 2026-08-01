module vlog47;
  reg [7:0] out [1:10];

  for (genvar i = 1; i <= 10; i++)
    initial out[i] = i;

  initial begin
    localparam limit = 10;
    #0;
    for (int i = 1; i <= limit; i++)
      if (out[i] !== i) begin
        $display("FAILED: out[%d] = %d", i, out[i]);
        $finish;
      end
    $display("PASSED");
  end
endmodule // vlog47
