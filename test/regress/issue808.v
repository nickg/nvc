module issue808;
  supply0 gnd;
  supply1 vcc;
  wire    out;

  assign gnd = vcc;

  GND uut (out);

  initial begin
    #1 $display("%x %x %x", gnd, vcc, out);
    $finish;
  end

endmodule // issue808

module GND (Y);
  output Y;
  supply0 Y ;
endmodule // GND
