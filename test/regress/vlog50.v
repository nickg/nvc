// Test upward hierarchical name resolution (IEEE 1364-2005 §12.4.2).
// A name inside a module that isn't found locally should be looked up
// in the parent (instantiating) module's scope.

module vlog50_target;
    reg [3:0] data = 4'h5;
endmodule

module vlog50_consumer;
    reg [3:0] shadow;
    initial begin
        #1;
        // "sibling" is a sibling instance in the parent scope.
        // Upward name resolution: not in local scope → look in parent.
        shadow = sibling.data;
        if (shadow !== 4'h5) begin
            $display("FAILED: shadow=%h expected 5", shadow);
            $finish;
        end
        $display("PASSED");
        $finish;
    end
endmodule

module vlog50;
    vlog50_target   sibling ();
    vlog50_consumer consumer ();
endmodule
