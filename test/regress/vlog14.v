module vlog14;

  wire x = 1;

  initial begin
    #1;

`ifdef FIRST_MACRO
    $display("FIRST MACRO WAS DEFINED OK !");
`endif

    $display(`SECOND_MACRO);

  end
endmodule // vlog14
