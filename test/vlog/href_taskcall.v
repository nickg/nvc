// Hier-path task-call disambiguation
module href_taskcall;

  sub u();

  initial begin
    u.u2.u3.my_task;            // OK
    u.u2.u3.my_task(1, 2);     // OK
    u.u2.u3.my_task();          // OK
  end

endmodule // href_taskcall
