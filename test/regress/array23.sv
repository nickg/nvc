module array23;

  bit [7:0] mem[];

  initial begin
    mem = new [5];

    $display("PASSED");
  end

endmodule // array23
