module disable1;
  reg x;
  initial begin
    begin : foo
      disable foo; // OK
      disable bar; // Error
      disable x;   // Error
    end
  end
endmodule // disable1
