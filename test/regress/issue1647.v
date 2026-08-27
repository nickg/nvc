module issue1647();
  integer cnt = 0;

  task test_task;
    parameter max_iter = 20;
    integer   i;
    begin
      for (i = 0; i < max_iter; i++)
        cnt += 1;
    end
  endtask // test_task

  initial
  begin
    test_task;
    if (cnt === 20)
      $display("PASSED");
    else
      $display("FAILED -- cnt=%d", cnt);
  end
endmodule // issue1647
