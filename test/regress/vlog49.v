// Test multi-level hierarchical references through cloned sibling
// instances of the same module, with a `define expanding to a
// concatenation used as RHS in an always block triggered by a
// hier-ref, and procedural writes through hier-refs.

module leaf;
    reg q = 0;
endmodule

module group;
    leaf a ();
    leaf b ();
    reg t = 0;
endmodule

`define PAIR {g.b.q, g.a.q}

module vlog49;
    group g ();
    reg [1:0] r = 0;

    always @(g.t) r <= `PAIR;

    initial begin
        #1;
        g.a.q = 1'b0;
        g.b.q = 1'b1;
        g.t = 1'b1;
        #1;
        if (r !== 2'b10) begin
            $display("FAILED: r=%b expected 10", r);
            $finish;
        end
        $display("PASSED");
        $finish;
    end
endmodule
