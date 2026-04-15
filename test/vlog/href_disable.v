// disable with hierarchical path
module href_disable;

  sub u();

  initial begin
    disable u.blk;              // OK
    disable u.inst.my_task;     // OK
    disable ;                   // Error
  end

endmodule // href_disable
