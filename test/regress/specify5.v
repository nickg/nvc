module specify5;

    reg ctrl = 1'b0;
    reg clk  = 1'b0;

    sub i_sub(
        .ctrl (ctrl),
        .clk  (clk)
    );

    assign #5 clk = ~clk;

    initial
    begin
        #3;
        ctrl = 1'b1;
        #4;
        ctrl = 1'b0;
        #6;
        ctrl = 1'b1;
        #7;
        ctrl = 1'b0;
        #5;
        $finish;
    end

endmodule

module sub(
    input ctrl,
    input clk
);

  specify
    // Check fires (recovery, integer limit):
    //      ctrl posedge: 3
    //      clk posedge:  5
    //      diff:         2
    //      limit:        3
    // Should fail at time 5. Fires again at time 15 for the same
    // reason (ctrl posedge at 13, clk posedge at 15, diff 2).
    $recovery(posedge ctrl, posedge clk, 3);

    // Check does not fire (recovery with limit = diff):
    //      ctrl posedge: 3
    //      clk posedge:  5
    //      diff:         2
    //      limit:        2
    // Should NOT fail (nor at time 15 for the same reason).
    $recovery(posedge ctrl, posedge clk, 2);

    // Check fires (removal):
    //      clk negedge:  10
    //      ctrl negedge: 7
    //      diff:         3
    //      limit:        4
    // Should fail at time 10. Does NOT fail at time 20, where clk
    // negedge and ctrl negedge happen simultaneously (diff 0).
    $removal(negedge clk, negedge ctrl, 4);

    // Check does not fire (removal with limit = diff):
    //      clk negedge:  10
    //      ctrl negedge: 7
    //      diff:         3
    //      limit:        3
    // Should NOT fail at time 10 (nor at time 20, simultaneous edges).
    $removal(negedge clk, negedge ctrl, 3);

  endspecify

endmodule
