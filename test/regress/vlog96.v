// Multi-instance outside-ref: two instances of the same module
// addressed from outside through the top-scope absolute path.
// vlog96.u1.data and vlog96.u2.data must resolve to distinct storage.
// Fills a gap: vlog49 exercises multi-instance clones referenced from
// inside a sibling scope; this test hits them from the top via rooted
// absolute paths.

module vlog96_cell;
    reg [7:0] data;
    initial data = 8'hFF;
endmodule

module vlog96;
    vlog96_cell u1 ();
    vlog96_cell u2 ();

    initial begin
        #1;
        // Rooted absolute paths addressing each clone distinctly.
        vlog96.u1.data = 8'h11;
        vlog96.u2.data = 8'h22;
        #1;

        if (vlog96.u1.data !== 8'h11) begin
            $display("FAILED: vlog96.u1.data=%h expected 11",
                     vlog96.u1.data);
            $finish;
        end
        if (vlog96.u2.data !== 8'h22) begin
            $display("FAILED: vlog96.u2.data=%h expected 22",
                     vlog96.u2.data);
            $finish;
        end

        // Verify unprefixed local names resolve to the same storage.
        if (u1.data !== 8'h11 || u2.data !== 8'h22) begin
            $display("FAILED: local u1.data=%h u2.data=%h (expected 11 22)",
                     u1.data, u2.data);
            $finish;
        end

        $display("PASSED");
        $finish;
    end
endmodule
