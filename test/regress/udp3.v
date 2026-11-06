// Test 'b' level symbol in UDP tables

primitive both_known(o, a, c);
  output o;
  input  a, c;
  table
  //a c : o
    b B : 1 ;
  endtable
endprimitive

primitive hold_known(q, d, en);
  output q;
  reg    q;
  input  d, en;
  table
  //d en : q : q+
    ? 0  : b : - ;
    ? 0  : x : 0 ;
    0 1  : ? : 0 ;
    1 1  : ? : 1 ;
  endtable
endprimitive

primitive edge_b(q, d, c);
  output q;
  reg    q;
  input  d, c;
  table
  //d  c   : q : q+
    ? (b1) : ? : 1 ;
    ? (bx) : ? : 0 ;
    ? (x1) : ? : x ;
    ? (?0) : ? : - ;
    *  ?   : ? : - ;
  endtable
endprimitive

module udp3;
  reg a, c, d, en, clk, failed;
  wire o, q1, q2;

  both_known u1(o, a, c);
  hold_known u2(q1, d, en);
  edge_b     u3(q2, d, clk);

  initial begin
    failed = 0;

    // Combinational: 'b' and 'B' match 0 or 1 but not x
    a = 0; c = 1; #1;
    if (o !== 1) failed = 1;
    a = 1; c = 0; #1;
    if (o !== 1) failed = 1;
    a = 1'bx; #1;
    if (o !== 1'bx) failed = 1;
    a = 1; c = 1'bx; #1;
    if (o !== 1'bx) failed = 1;

    // Sequential: 'b' as current state
    d = 1; en = 1; #1;
    if (q1 !== 1) failed = 1;
    en = 0; d = 0; #1;
    if (q1 !== 1) failed = 1;   // Held as current state matches 'b'

    // Edge indicators with 'b'
    clk = 0; #1;
    clk = 1; #1;
    if (q2 !== 1) failed = 1;   // (01) matches (b1)
    clk = 1'bx; #1;
    if (q2 !== 0) failed = 1;   // (1x) matches (bx)
    clk = 1; #1;
    if (q2 !== 1'bx) failed = 1;   // (x1) does not match (b1)

    if (failed)
      $display("FAILED");
    else
      $display("PASSED");
  end

endmodule
