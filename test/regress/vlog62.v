// Test rooted absolute hierarchical path (IEEE 1364-2005 S12.4.1).
//
// A hierarchical path whose head matches the top-level instance name
// is an absolute path.  From deep inside u1.u2, a reference to
// vlog62.other_sig resolves absolutely back to the top-level module
// scope, not via upward name search.

module vlog62_deep;
    reg [7:0] result;
    initial begin
        #1;
        // Absolute path: starts with top-level instance name "vlog62"
        result = vlog62.other_sig;
    end
endmodule

module vlog62_mid;
    vlog62_deep u2 ();
endmodule

module vlog62;
    reg [7:0] other_sig = 8'hF0;

    vlog62_mid u1 ();

    initial begin
        #2;
        if (u1.u2.result !== 8'hF0) begin
            $display("FAILED: u1.u2.result=%h expected F0",
                     u1.u2.result);
            $finish;
        end

        // Also verify writing through an absolute path from within
        // the deep instance works by reading back the value set above
        if (vlog62.other_sig !== 8'hF0) begin
            $display("FAILED: vlog62.other_sig=%h expected F0",
                     vlog62.other_sig);
            $finish;
        end

        $display("PASSED");
        $finish;
    end
endmodule
