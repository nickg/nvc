// Test for for-loop inside always @(*) sensitivity list
module vlog35;
  reg [3:0] a;
  reg [3:0] b;
  reg [3:0] result;
  integer i;

  always @(a or b)
  begin
    result = 0;
    for (i = 0; i < 4; i = i + 1)
    begin
      result[i] = a[i] ^ b[i];
    end
  end

  initial begin
    a = 4'b1010;
    b = 4'b0110;
    #1;
    $display("a=%b b=%b result=%b", a, b, result);
    if (result !== 4'b1100)
      $display("FAIL: expected 1100");

    a = 4'b1111;
    b = 4'b1111;
    #1;
    $display("a=%b b=%b result=%b", a, b, result);
    if (result !== 4'b0000)
      $display("FAIL: expected 0000");

    a = 4'b0000;
    b = 4'b0000;
    #1;
    $display("a=%b b=%b result=%b", a, b, result);
    if (result !== 4'b0000)
      $display("FAIL: expected 0000");

    $finish;
  end

endmodule
