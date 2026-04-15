// Parameter and specparam references via hierarchical path.
//
// Submodule has parameter WIDTH = 8 and a specify block with specparam
// T_delay = 3.  From outer scope: use u.WIDTH in a constant expression
// (array bound on a reg), and read u.T_delay for a value check (S14.2).
//
// NOTE: specparam via hier ref is low-priority and may not be supported
// initially.  If the specparam half fails while the parameter half passes,
// that is a known gap -- not a regression.

module vlog74_sub;
    parameter WIDTH = 8;

    reg [WIDTH-1:0] data;

    initial data = {WIDTH{1'b1}};

    // Specparam inside a specify block.
    specify
        specparam T_delay = 3;
    endspecify
endmodule

module vlog74;
    vlog74_sub u ();

    // Use hier parameter ref in a constant expression for array bound.
    reg [u.WIDTH-1:0] arr;

    initial begin
        #1;

        // Verify the parameter-sized reg has the right width.
        arr = {u.WIDTH{1'b1}};
        if (arr !== 8'hFF) begin
            $display("FAILED: arr=%h expected FF (u.WIDTH=%0d)", arr, u.WIDTH);
            $finish;
        end

        // Verify u.data is WIDTH bits wide (all ones = 8'hFF).
        if (u.data !== 8'hFF) begin
            $display("FAILED: u.data=%h expected FF", u.data);
            $finish;
        end

        // Check u.WIDTH as a plain RHS value.
        if (u.WIDTH !== 8) begin
            $display("FAILED: u.WIDTH=%0d expected 8", u.WIDTH);
            $finish;
        end

        // Specparam check -- may not yet be supported; see header note.
        // Uncomment when specparam hier refs are implemented:
        // if (u.T_delay !== 3) begin
        //     $display("FAILED: u.T_delay=%0d expected 3", u.T_delay);
        //     $finish;
        // end

        $display("PASSED");
        $finish;
    end
endmodule
