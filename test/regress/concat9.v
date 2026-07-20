module concat9;
  reg a, b, c;

  initial begin
    {{a, b}, c} = 3'b101;

    if ({a, b, c} != 3'b101)
      $display("FAILED");

    {{a, b}, c} <= 3'b010;
    #1;

    if ({a, b, c} == 3'b010)
      $display("PASSED");
    else
      $display("FAILED");
  end
endmodule
