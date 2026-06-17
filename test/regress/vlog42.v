module vlog42;
  integer a = 0, b = 0, c = 0;
  integer cnt = 0, body_cnt = 0;

  initial begin
    #1 a = 1;
    #1 a = 0;
    #1 a = 2;
    #1 b = 1;
    #1 c = 1;
  end

  initial begin
    // An initially true condition must not suspend.
    wait (a == 0) cnt += 1;

    // Re-evaluate the condition after each change until it becomes true.
    wait (a == 2) cnt += 2;

    // A compound expression must be sensitive to all of its inputs.
    wait (b && c) begin
      cnt += 4;
      body_cnt++;
    end

    // A null statement is also a valid wait body.
    wait (a == 2);
    cnt += 8;

    #1;

    if (cnt == 15 && body_cnt == 1)
      $display("PASSED");
    else
      $display("FAILED: cnt=%0d body_cnt=%0d", cnt, body_cnt);
  end

endmodule // vlog42
