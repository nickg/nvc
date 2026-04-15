// Function call on RHS via hierarchical reference (IEEE 1364 S10.3.3).
//
// Submodule has function my_func(a, b) = a + b.
// From outer scope: x = u.u2.my_func(8'h10, 8'h20); verify result is 8'h30.

module vlog73_inner;
    function [7:0] my_func(input [7:0] a, input [7:0] b);
        my_func = a + b;
    endfunction
endmodule

module vlog73_mid;
    vlog73_inner u2 ();
endmodule

module vlog73;
    vlog73_mid u ();

    reg [7:0] x;

    initial begin
        #1;
        x = u.u2.my_func(8'h10, 8'h20);
        #1;
        if (x !== 8'h30) begin
            $display("FAILED: x=%h expected 30", x);
            $finish;
        end

        // Second call with different args to confirm it's not a fluke.
        x = u.u2.my_func(8'hFF, 8'h01);
        #1;
        if (x !== 8'h00) begin
            $display("FAILED: x=%h expected 00 (8-bit wrap)", x);
            $finish;
        end

        $display("PASSED");
        $finish;
    end
endmodule
