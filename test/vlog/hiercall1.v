module sub1();
  reg r;
  function f(input x, input y = 0);
    f = x;
  endfunction
  task t;
  endtask
endmodule // sub1

module hiercall1();
  sub1 u1();
  reg x;

  initial begin
    x = u1.t(1);         // Error
    u1.f(1);             // Error
    x = u1.f();          // Error
    x = u1.f(1, 2, 3);   // Error
    x = u1.xxx(1);       // Error
    x = u1.f(1);         // OK
    x = u1.f(1, 0);      // OK
    u1.t;                // OK
  end

endmodule // hiercall1
