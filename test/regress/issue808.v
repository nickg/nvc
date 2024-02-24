module issue808;
  supply0 gnd;
  supply1 vcc;

  assign gnd = vcc;

  initial begin
    $display("%x %x", gnd, vcc);
    $finish;
  end

endmodule // issue808
