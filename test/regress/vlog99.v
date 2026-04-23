// Escaped identifiers in hierarchical paths (IEEE 1364-2005 §3.7.1 /
// IEEE 1800 §5.6.1).  Escaped idents begin with `\` and end with
// whitespace; the backslash and terminating whitespace are not part
// of the identifier.  Hier paths must accept them at head, mid, and
// tail positions.

module vlog99_leaf;
    reg [3:0] \cpu3 = 4'h3;
    reg [3:0] normal = 4'h5;
endmodule

module vlog99;
    // Instance declared with escaped identifier name.
    vlog99_leaf \top_leaf ();
    vlog99_leaf u ();

    initial begin
        #1;

        // (a) Escaped ident at mid: u.\cpu3
        if (u.\cpu3 !== 4'h3) begin
            $display("FAILED: u.\\cpu3 =%h expected 3", u.\cpu3 );
            $finish;
        end

        // (b) Escaped ident at head: \top_leaf .normal
        if (\top_leaf .normal !== 4'h5) begin
            $display("FAILED: \\top_leaf .normal=%h expected 5",
                     \top_leaf .normal);
            $finish;
        end

        // (c) Write through an escaped-ident hier path
        \top_leaf .normal = 4'h9;
        #1;
        if (\top_leaf .normal !== 4'h9) begin
            $display("FAILED: write \\top_leaf .normal=%h expected 9",
                     \top_leaf .normal);
            $finish;
        end

        $display("PASSED");
        $finish;
    end
endmodule
