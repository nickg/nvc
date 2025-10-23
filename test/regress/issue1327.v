module sub #(parameter p = "123");
  initial
    if (p === "hello")
      $display("PASSED");
    else
      $display("FAILED");
endmodule // sub

module issue1327;
  sub #(.p("hello")) u();
endmodule // issue1327
