// Test SystemVerilog $unit::name compilation-unit scope reference
// (IEEE 1800-2017 S3.12.1, S26.3).
//
// A localparam declared at the file level outside any module is in
// the compilation-unit scope.  It is accessible from inside a module
// body via $unit::THE_NAME.

localparam [7:0] CU_PARAM = 8'hE7;
localparam [15:0] CU_WIDE = 16'hCAFE;

module vlog64_inner;
    reg [7:0] p;
    reg [15:0] w;
    initial begin
        p = $unit::CU_PARAM;
        w = $unit::CU_WIDE;
    end
endmodule

module vlog64;
    vlog64_inner u ();

    initial begin
        #1;
        if (u.p !== 8'hE7) begin
            $display("FAILED: u.p=%h expected E7", u.p);
            $finish;
        end
        if (u.w !== 16'hCAFE) begin
            $display("FAILED: u.w=%h expected CAFE", u.w);
            $finish;
        end

        // Also verify direct $unit:: access from top-level module
        if ($unit::CU_PARAM !== 8'hE7) begin
            $display("FAILED: $unit::CU_PARAM=%h expected E7",
                     $unit::CU_PARAM);
            $finish;
        end

        $display("PASSED");
        $finish;
    end
endmodule
