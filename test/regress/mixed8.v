module sub(clk, pass);
  input clk;
  output reg pass;

  initial pass = 1;

  always @(posedge clk) begin
    $display("%t: posedge clk", $time);
    if ($time == 0) begin
      $display("FAILED -- posedge at time zero");
      pass = 0;
    end
  end

endmodule // sub

module mid(clk, pass);
  input clk;
  output pass;

  sub u(clk, pass);
endmodule // mid

module top(clk, pass);
  input clk;
  output pass;

  mid u(clk, pass);
endmodule // top
