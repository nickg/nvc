// tri0 and tri1 nets are pulled when nothing drives them
module vtripull1_sub (a, b, v, y);
  input a, b;
  input [3:0] v;
  output y;
  tri1 a;
  tri0 b;
  tri1 [3:0] v;
  assign y = a & ~b & (v == 4'hf);
endmodule

module vtripull1;
  reg  drv = 0;
  wire y1, y2;
  tri0 t0;
  tri1 [7:0] t1;
  tri1 [69:0] wide;
  wire ok;

  vtripull1_sub u1 (.y(y1));                        // All inputs open
  vtripull1_sub u2 (.a(drv), .b(), .v(), .y(y2));   // a driven 0

  reg  r2 = 0;                                      // Two instances
  wire y3, y4;
  vtripull1_sub u3 (.a(r2), .b(1'b0), .v(4'hf), .y(y3));
  vtripull1_sub u4 (.a(r2), .y(y4));

  assign t1[3:0] = 4'b0101;                         // Strong beats pull
  assign (weak0, weak1) wide[0] = 1'b0;             // Pull beats weak

  initial begin
    #1;
    if (y1 !== 1'b1 || y2 !== 1'b0) $display("FAILED y %b %b", y1, y2);
    if (y3 !== 1'b0 || y4 !== 1'b0) $display("FAILED r2 %b %b", y3, y4);
    if (t0 !== 1'b0) $display("FAILED t0 %b", t0);
    if (t1 !== 8'b11110101) $display("FAILED t1 %b", t1);
    if (wide !== {70{1'b1}}) $display("FAILED wide %b", wide);
    drv = 1;
    #1;
    if (y2 !== 1'b1) $display("FAILED y2 %b", y2);
    $display("PASSED");
  end
endmodule
