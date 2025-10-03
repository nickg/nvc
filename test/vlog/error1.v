module error1;
  localparam foo bar = 0; // Error
endmodule // error1

module error2;
endmodule : error1   // Error
