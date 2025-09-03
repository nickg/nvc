module label1;
  reg r;

  initial
    name: fork  // OK
      r = 1;
    join: name

  initial
    name: fork : name  // Error
      r = 1;
    join: name

  initial
    name: begin  // OK
      r = 1;
    end: name

  initial
    name: begin : name  // Error
      r = 1;
    end: name

endmodule // label1
