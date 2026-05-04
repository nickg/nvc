module tfcall6;
  reg [3:0] mem [0:1] [0:1];

  task cycle;
    input a, b;
    reg [3:0] tmp;
    begin
      tmp = 4'd2;
      mem[a][b] = 4'd5;
      $display("tmp=%d mem=%d", tmp, mem[a][b]);
    end
  endtask

  initial begin
    cycle(1'b0, 1'b0);
    $display("PASSED");
  end
endmodule
