// Continuous assign and procedural continuous assign with hier ref as LHS.
//
// Module-level continuous assign: assign u.x = y;
// Procedural continuous assign (inside initial): assign u.x = y;
// followed by deassign u.x; (IEEE 1364 S9.3.1).

module vlog67_sub;
    reg [7:0] x = 8'h00;
    reg [7:0] z = 8'h00;
endmodule

module vlog67;
    reg [7:0] y = 8'hAB;

    vlog67_sub u ();

    // Module-level continuous assign with hier ref LHS.
    assign u.z = y;

    initial begin
        #1;
        // Continuous assign should have driven u.z by now.
        if (u.z !== 8'hAB) begin
            $display("FAILED: continuous assign u.z=%h expected AB", u.z);
            $finish;
        end

        // Procedural continuous assign: override u.x.
        assign u.x = y;
        #1;
        if (u.x !== 8'hAB) begin
            $display("FAILED: procedural assign u.x=%h expected AB", u.x);
            $finish;
        end

        // Change driving value; procedural continuous tracks it.
        y = 8'hCD;
        #1;
        if (u.x !== 8'hCD) begin
            $display("FAILED: tracking u.x=%h expected CD", u.x);
            $finish;
        end

        // Deassign: release the procedural continuous override.
        deassign u.x;
        u.x = 8'hFF;
        #1;
        if (u.x !== 8'hFF) begin
            $display("FAILED: after deassign u.x=%h expected FF", u.x);
            $finish;
        end

        $display("PASSED");
        $finish;
    end
endmodule
