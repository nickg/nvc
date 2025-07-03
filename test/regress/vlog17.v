module sub(o);
  output o;
  parameter p = 0;
  generate
    if (p == 42) begin
      assign o = 1;
    end else begin
      assign o = 0;
    end
  endgenerate
endmodule // sub

module vlog17;
  wire o1, o2, o3;

  sub #(42) u1(o1);
  sub #(56) u2(o2);
  sub #('hx) u3(o3);

  initial begin
    #1 $display("%x %x %x", o1, o2, o3);
    if (o1 === 1 && o2 === 0 && o3 === 0)
      $display("PASSED");
    else
      $display("FAILED");
  end

endmodule // vlog17
