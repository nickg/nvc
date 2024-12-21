module vlog14;

  wire x = 1;

  initial begin
    #1;

`ifdef MACRO_A
    $display("MACRO_A WAS DEFINED OK !");
`endif

    $display(`MACRO_B);

  end
endmodule // vlog14
