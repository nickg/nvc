module sub #(parameter W = 4) (input [W-1:0] a, input [W-1:0] b, input en,
                              output [W-1:0] y_and, output [W-1:0] y_buf,
                              output [W-1:0] y_tri);
  and g_and[W-1:0] (y_and, a, b);    // vector terminals: one bit each
  buf g_buf[0:W-1] (y_buf, a);       // ascending range
  bufif1 g_tri[W-1:0] (y_tri, b, en);   // scalar enable for every gate
endmodule

module vgatearr1;
  reg [5:0] a = 6'b101100, b = 6'b111010;
  reg en = 0, s = 1;
  wire [5:0] y_and, y_buf, y_tri;
  wire [2:0] n, p;

  sub #(6) u (a, b, en, y_and, y_buf, y_tri);

  nand g_nand[2:0] (n, a[2:0], s);   // scalar input for every gate
  pullup g_pu[2:0] (p);

  initial begin
    #1;
    if (y_and !== 6'b101000) $display("FAILED and %b", y_and);
    else if (y_buf !== 6'b101100) $display("FAILED buf %b", y_buf);
    else if (y_tri !== 6'bzzzzzz) $display("FAILED tri %b", y_tri);
    else if (n !== 3'b011) $display("FAILED nand %b", n);
    else if (p !== 3'b111) $display("FAILED pullup %b", p);
    else begin
      en = 1;
      s = 0;
      #1;
      if (y_tri !== 6'b111010) $display("FAILED tri %b", y_tri);
      else if (n !== 3'b111) $display("FAILED nand %b", n);
      else $display("PASSED");
    end
  end
endmodule
