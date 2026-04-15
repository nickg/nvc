// Named events accessed through hierarchical references.
//
// Blocking trigger:    -> u.ev;       (IEEE 1364 S9.7.3)
// Nonblocking trigger: ->> u.ev;      (IEEE 1800 S9.4.2.1)
// Event wait:          @(u.ev)
// Triggered query:     u.ev.triggered

module vlog69_sub;
    event ev;
endmodule

module vlog69;
    vlog69_sub u ();

    reg [2:0] seen = 3'b000;

    // Consumer 1: blocking @(u.ev), sets seen[0].
    initial begin
        @(u.ev);
        seen[0] = 1;
    end

    // Consumer 2: poll u.ev.triggered, sets seen[1].
    initial begin
        wait (u.ev.triggered);
        seen[1] = 1;
    end

    // Consumer 3: second @(u.ev) for the nonblocking trigger round,
    // sets seen[2].
    initial begin
        // Skip the first trigger.
        @(u.ev);
        @(u.ev);
        seen[2] = 1;
    end

    // Driver: blocking trigger first, then nonblocking.
    initial begin
        #10;
        -> u.ev;    // Blocking trigger.
        #10;
        ->> u.ev;   // Nonblocking trigger.
    end

    // Checker.
    initial begin
        #15;
        if (seen[0] !== 1) begin
            $display("FAILED: blocking @(u.ev) did not fire, seen=%b", seen);
            $finish;
        end
        if (seen[1] !== 1) begin
            $display("FAILED: u.ev.triggered not set, seen=%b", seen);
            $finish;
        end

        #15;
        if (seen[2] !== 1) begin
            $display("FAILED: nonblocking ->> u.ev did not fire, seen=%b", seen);
            $finish;
        end

        $display("PASSED");
        $finish;
    end
endmodule
