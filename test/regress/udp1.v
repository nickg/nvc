primitive u_dff(q,d,c);
  output q;
  reg	 q;
  input	 d,c;
  table
  //d c : q : q+
    0 p : ? : 0 ;
    1 p : ? : 1 ;
    ? n : ? : - ;
    * ? : ? : - ;
  endtable
endprimitive

module udp1;
  reg clk, d, failed;
  wire q;

  u_dff ff0(q, d, clk);

  always #1 clk = ~clk;

  initial begin
    clk = 0;
    d = 0;
    failed = 0;

    @(negedge clk);
    if (q !== 0) failed = 1;
    d = 1;

    @(negedge clk);
    if (q !== 1) failed = 1;
    d = 1'bx;

    @(negedge clk);
    if (q !== 1'bx) failed = 1;
    d = 0;

    @(negedge clk);
    if (q !== 0) failed = 1;

    #1;
    if (failed)
      $display("FAILED");
    else
      $display("PASSED");
    $finish;
  end // initial begin

  always @(*)
    $display("%t %x %x | %x", $time, clk, d, q);

endmodule
