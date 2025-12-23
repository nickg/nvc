module sub(load, addr, dout);
  input load;
  input [4:0] addr;
  output [15:0] dout;
  reg [15:0] array [0:31];

  assign dout = array[addr];

  initial begin
    @(posedge load);
    $display("loading data...");
    $readmemh("data.hex", array);
  end

endmodule // sub
