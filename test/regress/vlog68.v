// Test wait and wait_order with hierarchical references.
//
// wait(u.x) blocks until a hier-addressed reg goes high (IEEE 1364 S9.7.4).
// wait_order(u.a, u.b) checks ordered event arrivals (IEEE 1800 S9.4.3).

module vlog68_sub;
    reg x = 0;
    event a;
    event b;
endmodule

module vlog68;
    vlog68_sub u ();

    reg wait_done = 0;
    reg order_done = 0;

    // Consumer: block on wait(u.x).
    initial begin
        wait (u.x);
        wait_done = 1;
    end

    // Driver: raise u.x after a delay.
    initial begin
        #10;
        u.x = 1;
    end

    // Consumer: wait_order on hier-addressed named events.
    initial begin
        wait_order (u.a, u.b);
        order_done = 1;
    end

    // Driver: trigger u.a then u.b in that order.
    initial begin
        #20;
        -> u.a;
        #5;
        -> u.b;
    end

    // Checker.
    initial begin
        #15;
        if (wait_done !== 1) begin
            $display("FAILED: wait(u.x) did not fire, wait_done=%0d", wait_done);
            $finish;
        end

        #20;
        if (order_done !== 1) begin
            $display("FAILED: wait_order did not complete, order_done=%0d", order_done);
            $finish;
        end

        $display("PASSED");
        $finish;
    end
endmodule
