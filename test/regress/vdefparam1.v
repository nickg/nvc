module vdefparam1_leaf;
  parameter W = 1;
  parameter S = "none";
  parameter D = W * 2;     // Depends on an overridden parameter
endmodule

module vdefparam1_mid;
  parameter K = 3;
  vdefparam1_leaf a ();
  vdefparam1_leaf b ();
  defparam a.W = K + 1;    // Value from the enclosing scope
endmodule

module vdefparam1;
  genvar i;

  vdefparam1_leaf l1 (), l2 ();
  defparam l1.W = 8, l1.S = "set";

  vdefparam1_leaf #(.W(4)) l3 ();
  defparam l3.W = 5;       // Wins over the instance parameter value

  vdefparam1_mid m1 ();
  vdefparam1_mid m2 ();
  defparam m2.K = 10;
  defparam m2.b.W = 7;     // Through two levels of instances

  if (1) begin : g
    vdefparam1_leaf u ();
  end
  defparam g.u.W = 9;      // Into a generate block

  for (i = 0; i < 2; i = i + 1) begin : gen
    vdefparam1_leaf u ();
  end
  defparam gen[1].u.W = 11;

  defparam vdefparam1.l2.S = "self";   // Name starting with this module

  initial begin
    if (l1.W !== 8 || l1.S != "set" || l1.D !== 16)
      $display("FAILED l1 %0d %s %0d", l1.W, l1.S, l1.D);
    else if (l2.W !== 1 || l2.S != "self")
      $display("FAILED l2 %0d %s", l2.W, l2.S);
    else if (l3.W !== 5)
      $display("FAILED l3 %0d", l3.W);
    else if (m1.a.W !== 4 || m1.b.W !== 1)
      $display("FAILED m1 %0d %0d", m1.a.W, m1.b.W);
    else if (m2.a.W !== 11 || m2.b.W !== 7)
      $display("FAILED m2 %0d %0d", m2.a.W, m2.b.W);
    else if (g.u.W !== 9)
      $display("FAILED g.u %0d", g.u.W);
    else if (gen[0].u.W !== 1 || gen[1].u.W !== 11)
      $display("FAILED gen %0d %0d", gen[0].u.W, gen[1].u.W);
    else
      $display("PASSED");
  end
endmodule
