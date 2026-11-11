// Sequential UDP with zero delay feedback from output to input

primitive udp4_dff (q, clk, d);
  output q;
  reg    q;
  input  clk, d;
  table
  //clk  d : q : q+
    (01) 0 : ? : 0 ;
    (01) 1 : ? : 1 ;
    (?0) ? : ? : - ;
     ?   * : ? : - ;
  endtable
endprimitive

module udp4;
  reg  clk = 0, rst = 1, failed = 0;
  wire q0, q1, d0, d1, t;

  // Two bit counter: d0 = rst ? 0 : ~q0, d1 = rst ? 0 : q1 ^ q0
  udp4_dff ff0 (q0, clk, d0);
  udp4_dff ff1 (q1, clk, d1);
  nor  g0 (d0, q0, rst);
  xor  g1 (t, q1, q0);
  and  g2 (d1, t, ~rst);

  always #5 clk = ~clk;

  integer i;
  initial begin
    #7 rst = 0;    // Both flops cleared by the edge at 5
    for (i = 0; i < 6; i = i + 1) begin
      @(negedge clk);   // Counts on posedges at 15, 25, ...
      if ({q1, q0} !== i % 4) begin
        $display("FAILED: count %b expected %0d", {q1, q0}, i % 4);
        failed = 1;
      end
    end

    if (failed)
      $display("FAILED");
    else
      $display("PASSED");
    $finish;
  end

endmodule
