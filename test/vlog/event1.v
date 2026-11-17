module event1;
  event a, b;
  event e;
  reg x;

  function automatic integer trig;
    input dummy;
    begin
      -> e;                     // OK
      trig = 1;
    end
  endfunction

  initial begin
    -> e;                       // OK
    ->> e;                      // OK
    -> x;                       // Error
    ->> x;                      // Error
    ->> #1 e;                   // Error
    -> a;                       // OK
    -> b;                       // OK
    x = e.triggered;            // Error
    @(e);                       // OK
  end
endmodule // event1
