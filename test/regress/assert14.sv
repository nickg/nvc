module assert14;
  integer x = 42;

  initial begin
    assert (x == 42);  // OK
    assert (x == 5) else $warning("x = %d", x);
    assert (x < 100) x = 1; else x = 2;
    assert (x == 2) $display("pass");  // Error
  end

endmodule // assert14
