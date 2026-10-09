module vlog48;

  reg a = 1'b0;
  reg b = 1'b0;
  integer count = 0;

  // Sensitivity list with two edge-qualified events lowers to an OR of
  // two @posedge/@negedge function triggers. The always block must only
  // wake on a's posedge or b's negedge, never on b's posedge.
  always @(posedge a or negedge b)
    count <= count + 1;

  assign #5 b = ~b;   // posedge @5,15,25,...  negedge @10,20,30,...

  initial begin
    #4;
    a <= 1'b1;         // one real posedge of a at 4fs
    #18;                // now at 22fs: b has toggled at 5, 10, 15, 20
    if (count !== 3) begin
      $display("FAILED: count=%d expected 3", count);
      $fatal;
    end
    $display("PASSED");
    $finish;
  end

endmodule // vlog48