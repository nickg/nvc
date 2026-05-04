module display3;
  reg [3:0] u;
  reg signed [3:0] s;
  reg [3:0] mem [0:1];

  initial begin
    u = 4'd15;
    s = 4'd15;
    mem[0] = 4'd15;

    $display("u=%d s=%d mem=%d", u, s, mem[0]);
  end
endmodule
