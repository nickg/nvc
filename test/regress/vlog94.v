// wait(u.ev.triggered) — level-sensitive wait consuming a
// hier-addressed named event's triggered property.  Fills a gap left
// by vlog69 which covers @(u.ev) and .triggered query but not the
// wait+triggered combination explicitly listed in the design doc.

module vlog94_src;
    event ev;
endmodule

module vlog94;
    vlog94_src u ();
    reg fired = 0;

    // Trigger the hier event after a delay.
    initial begin
        #5 -> u.ev;
    end

    // Consumer: block on the .triggered property via a hier ref.
    initial begin
        wait (u.ev.triggered);
        fired = 1;
    end

    // Watchdog: verify the wait fired.
    initial begin
        #20;
        if (!fired) begin
            $display("FAILED: wait(u.ev.triggered) never returned");
            $finish;
        end
        $display("PASSED");
        $finish;
    end
endmodule
