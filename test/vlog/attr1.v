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
endmodule // mod1
