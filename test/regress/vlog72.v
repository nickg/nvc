// Task call via hierarchical reference through 3+ levels of hierarchy.
//
// Hierarchy: vlog72 -> u1 -> u2 -> u3.
// u3 has task no_args (no parentheses form, S10.2.2) and task my_task(args).
// From vlog72, call both forms and verify side effects.

module vlog72_l3;
    reg [7:0] result = 8'h00;
    reg        flag  = 0;

    task no_args;
        flag = 1;
    endtask

    task my_task(input [3:0] v);
        result = {4'h0, v};
    endtask
endmodule

module vlog72_l2;
    vlog72_l3 u3 ();
endmodule

module vlog72_l1;
    vlog72_l2 u2 ();
endmodule

module vlog72;
    vlog72_l1 u1 ();

    initial begin
        #1;

        // No-parenthesis task call through 3 levels.
        u1.u2.u3.no_args;
        #1;
        if (u1.u2.u3.flag !== 1) begin
            $display("FAILED: no_args flag=%0d expected 1", u1.u2.u3.flag);
            $finish;
        end

        // Task call with arguments through 3 levels.
        u1.u2.u3.my_task(4'h5);
        #1;
        if (u1.u2.u3.result !== 8'h05) begin
            $display("FAILED: my_task result=%h expected 05",
                     u1.u2.u3.result);
            $finish;
        end

        $display("PASSED");
        $finish;
    end
endmodule
