module select24;
  initial begin
    reg [95:0] wide;
    reg [15:0] part;
    reg [79:0] bigpart;
    integer    base;

    wide = 96'h0123456789abcdef01234567;
    base = 32;
    part = wide[48:32];
    bigpart = wide[95:16];

    if (part !== 16'hcdef)
      $display("FAILED: expected cdef, got %h", part);
    else if (bigpart !== 80'h0123456789abcdef0123)
      $display("FAILED: expected 0123456789abcdef0123, got %h", bigpart);
    else
      $display("PASSED");
  end
endmodule
