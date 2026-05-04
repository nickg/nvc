module issue1516_sub(load, addr, dout);
  input load;
  input [1:0] addr;
  output [49:0] dout;
  reg [49:0] array [0:3];

  assign dout = array[addr];

  initial begin
    @(posedge load);
    $readmemh("issue1516.hex", array);
  end
endmodule
