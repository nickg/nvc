module vlog54;
  event e1, e2;
  integer n1, n2, n3, n4;

  initial begin : waiter1
    n1 = 0;
    forever begin
      @(e1);
      n1 = n1 + 1;
    end
  end

  initial begin : waiter2
    n2 = 0;
    forever begin
      @(e1);
      n2 = n2 + 1;
    end
  end

  initial begin : waiter3
    n3 = 0;
    @(e2);
    n3 = n3 + 1;
  end

  task trigger_e2;
    -> e2;
  endtask

  // The event is triggered before this process waits on it in the same time
  // step so it is missed
  integer n5;
  initial begin : late_waiter
    n5 = 0;
    #20;
    -> e1;
    @(e1);
    n5 = n5 + 1;
  end

  vlog54_sub u();

  integer n6 = 0;
  initial begin : hier_waiter
    @(u.s);
    n6 = n6 + 1;
  end

  initial begin : trigger
    #1 -> e1;
    #1 -> e1;
    #1;
    if (n1 !== 2 || n2 !== 2) begin
      $display("FAILED: n1=%0d n2=%0d", n1, n2);
      $fatal;
    end

    // Re-trigger loop
    repeat (5) begin
      -> e1;
      #1;
    end
    if (n1 !== 7 || n2 !== 7) begin
      $display("FAILED: loop n1=%0d n2=%0d", n1, n2);
      $fatal;
    end

    // Waiter that starts after the trigger misses it
    -> e2;
    #1;
    if (n3 !== 1) begin
      $display("FAILED: n3=%0d", n3);
      $fatal;
    end
    -> e2;
    #1;
    if (n3 !== 1) begin
      $display("FAILED: n3 late=%0d", n3);
      $fatal;
    end

    // Trigger from a task
    trigger_e2;
    #1;
    if (n3 !== 1) begin
      $display("FAILED: task n3=%0d", n3);
      $fatal;
    end

    // Non-blocking trigger
    ->> e1;
    #1;
    if (n1 !== 8) begin
      $display("FAILED: nb n1=%0d", n1);
      $fatal;
    end
    #20;
    if (n5 !== 0) begin
      $display("FAILED: n5=%0d", n5);
      $fatal;
    end

    // Several triggers in one time step wake a waiter once
    n1 = 0;
    -> e1;
    -> e1;
    #1;
    if (n1 !== 1) begin
      $display("FAILED: double n1=%0d", n1);
      $fatal;
    end
    ->> e1;
    ->> e1;
    #1;
    if (n1 !== 2) begin
      $display("FAILED: double nb n1=%0d", n1);
      $fatal;
    end

    // Hierarchical trigger
    -> u.s;
    #1;
    if (n6 !== 1) begin
      $display("FAILED: n6=%0d", n6);
      $fatal;
    end

    $display("PASSED");
    $finish;
  end
endmodule

module vlog54_sub;
  event s;
endmodule
