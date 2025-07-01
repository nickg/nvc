module initial1;
  reg z = 0;
  initial begin : label0
    reg w;
    (* attr0 *) reg e;
    w = 1;
    #1 z = 1;
    $display("ok");
    reg q = 0; // Error declarative part must be at the beginnig
  end : label0
  initial begin : label1
  end // label1
  initial begin
  end : label2 // error
  initial begin : a_label
  end : another_label // error

  initial fork : fork0
    (* attr0 *) reg w;
    reg e;
    w = 1;
    reg q = 0; // Error declarative part must be at the beginnig
    #2 z = 2;
  join : fork0
  initial fork : fork1
  join // fork1
  initial fork
  join : fork3 // Error
  initial fork : fork4
  join : fork5 // Error

endmodule // initial1
