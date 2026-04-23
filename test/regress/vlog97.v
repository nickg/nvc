// Elaboration-ordering: hierarchical reference in a constant
// expression consumed during elaboration, where the prefix instance
// has not yet finished elaborating at first-pass resolution time.
//
// Exercises the deferred-resolution constraint from the design doc:
//   "These must resolve before the consuming construct is evaluated,
//    even if the prefix instance hasn't finished elaborating yet. The
//    unified resolver's best-effort first pass must cover this; if a
//    ref is unresolvable before its prefix, elaboration must defer
//    rather than emit a spurious error."
//
// Here: u.MODE drives a generate condition, and u.WIDTH sizes a reg
// inside the selected generate branch.  Both consume u's parameters
// at elab time.

module vlog97_src;
    parameter [1:0] MODE  = 2'b01;
    parameter       WIDTH = 6;
endmodule

module vlog97;
    vlog97_src u ();

    generate
        if (u.MODE == 2'b01) begin : sel
            reg [u.WIDTH-1:0] bits = 6'b010101;
        end
    endgenerate

    initial begin
        #1;
        if (sel.bits !== 6'b010101) begin
            $display("FAILED: sel.bits=%b expected 010101", sel.bits);
            $finish;
        end
        $display("PASSED");
        $finish;
    end
endmodule
