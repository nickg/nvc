module issue1469;
  localparam [1:0] p = 2'b10;
  wire n1;
  wire n2;

  parameter [1:0] q = 2'b01;
  wire n3;

  // works
  //assign n1 = p[0:0];
  //assign n2 = p[1:1];

  // does not work
  assign n1 = p[0];
  assign n2 = p[1];

  assign n3 = q[0];

  initial begin
    if (n1 != 1'b0 || n2 != 1'b1 || n3 != 1'b1) begin
      $display("FAILED");
    end
    else begin
      $display("PASSED");
    end
  end
endmodule // lparam
