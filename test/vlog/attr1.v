(* foo *)  // OK
(* bar *)  // OK
module mod1((* foo *) output o);
  (* x *) reg x;  // OK
  task task1;
    (* a *) int p; // OK
    foo: (* b *) p = 5; // OK
  endtask // task1
  task task2;
    (* a *) int p; // OK
    (* error *) bar: (* b *) p = 6; // Error
  endtask // task2
  sub u1((* foo *) x, (* bar *) .p(x)); // OK
  initial x <= 1 + (* c *) x; // OK
  function func;
    input p;
    task1 (* xx *); // OK
    func = ~ (* yy *) p; // OK
  endfunction // func
  initial x = func (* d *) (x); // OK
endmodule // mod1
