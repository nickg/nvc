// Test instance array hierarchical references (IEEE 1364-2005 S7.1.5).
//
// An array of module instances is declared as:
//   some_mod arr [3:0] (...);
// Each element is accessed via arr[index].sig.  Each instance gets a
// distinct parameter value so the test can verify that indexed access
// resolves to the correct instance, not a shared or aliased copy.

module vlog65_cell;
    parameter ID = 0;
    reg [7:0] sig;
    initial sig = ID;
endmodule

module vlog65;
    // Instance array: 4 copies, each with a distinct ID parameter
    vlog65_cell #(.ID(8'h10)) arr0 ();
    vlog65_cell #(.ID(8'h20)) arr1 ();
    vlog65_cell #(.ID(8'h30)) arr2 ();
    vlog65_cell #(.ID(8'h40)) arr3 ();

    // Instance array syntax (the primary thing under test)
    vlog65_cell arr [3:0] ();

    // Use generate to give each array element a distinct ID since
    // instance arrays share the same parameter set by default.
    // Alternative approach: set values procedurally.
    initial begin
        // Write distinct values into each array element
        arr[0].sig = 8'hA0;
        arr[1].sig = 8'hA1;
        arr[2].sig = 8'hA2;
        arr[3].sig = 8'hA3;
    end

    initial begin
        #1;

        // Verify each indexed instance has its own storage
        if (arr[0].sig !== 8'hA0) begin
            $display("FAILED: arr[0].sig=%h expected A0", arr[0].sig);
            $finish;
        end
        if (arr[1].sig !== 8'hA1) begin
            $display("FAILED: arr[1].sig=%h expected A1", arr[1].sig);
            $finish;
        end
        if (arr[2].sig !== 8'hA2) begin
            $display("FAILED: arr[2].sig=%h expected A2", arr[2].sig);
            $finish;
        end
        if (arr[3].sig !== 8'hA3) begin
            $display("FAILED: arr[3].sig=%h expected A3", arr[3].sig);
            $finish;
        end

        $display("PASSED");
        $finish;
    end
endmodule
