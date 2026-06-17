module string4;
  parameter STYLE = "AUTO";
  parameter STYLE_INT = (STYLE == "AUTO") ? "REDUCTION" : STYLE;
  reg [8*9-1:0] style_reg = "REDUCTION";

  generate
    if (STYLE_INT == "REDUCTION") begin : style_ok
    end else begin : style_bad
      initial $display("FAILED");
    end

    if ("REDUCTION" == "REDUCTION") begin : direct_ok
    end else begin : direct_bad
      initial $display("FAILED");
    end
  endgenerate

  initial begin
    if ("REDUCTION" != "REDUCTION")
      $display("FAILED");

    if ("REDUCTION" == "REDUCTIOM")
      $display("FAILED");

    if (style_reg != "REDUCTION")
      $display("FAILED");

    if (style_reg == "REDUCTIOM")
      $display("FAILED");

    $display("PASSED");
  end
endmodule
