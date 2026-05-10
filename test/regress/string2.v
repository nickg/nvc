module string2;

  parameter FILE = "cmem_4096.hex";

  initial begin
    $display("%s", FILE);
    $display("PASSED");
  end

endmodule
