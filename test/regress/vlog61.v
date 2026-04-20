// Test generate-block hierarchical references (IEEE 1364-2005 S12.4,
// S12.4.3 generate scope; IEEE 1800 S27).
//
// Covers:
//   1. for-generate with genvar index: gen_loop[i].u.x
//   2. if-generate with named block: gen_if.u.x
//   3. Named block inside a generate construct: gen_named.inner.val
//   4. Nested if-generate accessing module-level ports (upref through
//      generate scopes) — regression for param-dependent port widths
//
// Exercises mid-path constant_bit_select in the hierarchical path.

module vlog61_leaf;
    parameter INIT = 0;
    reg [7:0] x;
    initial x = INIT;
endmodule

module vlog61_named_leaf;
    reg [7:0] val = 8'hCC;
endmodule

// 4. Module with nested if-generate referencing module-level ports.
//    Exercises var-upref through multiple generate scopes back to
//    the enclosing module's port variables.
module vlog61_mux #(
    parameter SEL = 0,
    parameter W   = 1
)(
    input  wire [W-1:0] a,
    input  wire [W-1:0] b,
    output wire [W-1:0] y
);
    wire [W-1:0] tmp;

    if (SEL == 0) begin : gen_path_a
        if (W == 1) begin : gen_narrow
            assign tmp = a;
        end
        else begin : gen_wide
            assign tmp = a;
        end
    end
    else begin : gen_path_b
        assign tmp = b;
    end

    assign y = tmp;
endmodule

module vlog61;
    genvar i;

    // 1. for-generate: creates gen_loop[0] .. gen_loop[3]
    generate
        for (i = 0; i < 4; i = i + 1) begin : gen_loop
            vlog61_leaf #(.INIT(i * 10)) u ();
        end
    endgenerate

    // 2. if-generate with named block
    parameter USE_A = 1;
    generate
        if (USE_A) begin : gen_if
            vlog61_leaf #(.INIT(8'hAB)) u ();
        end else begin : gen_else
            vlog61_leaf #(.INIT(8'hEF)) u ();
        end
    endgenerate

    // 3. Named block inside generate
    generate
        begin : gen_named
            vlog61_named_leaf inner ();
        end
    endgenerate

    // 4. Nested if-generate accessing module-level ports
    reg mux_a, mux_b;
    wire mux_y;
    vlog61_mux #(.SEL(0), .W(1)) mux0 (
        .a(mux_a), .b(mux_b), .y(mux_y)
    );

    initial begin
        #1;

        // Check for-generate indexed access
        if (gen_loop[0].u.x !== 8'd0) begin
            $display("FAILED: gen_loop[0].u.x=%0d expected 0",
                     gen_loop[0].u.x);
            $finish;
        end
        if (gen_loop[2].u.x !== 8'd20) begin
            $display("FAILED: gen_loop[2].u.x=%0d expected 20",
                     gen_loop[2].u.x);
            $finish;
        end
        if (gen_loop[3].u.x !== 8'd30) begin
            $display("FAILED: gen_loop[3].u.x=%0d expected 30",
                     gen_loop[3].u.x);
            $finish;
        end

        // Check if-generate
        if (gen_if.u.x !== 8'hAB) begin
            $display("FAILED: gen_if.u.x=%h expected AB", gen_if.u.x);
            $finish;
        end

        // Check named block inside generate
        if (gen_named.inner.val !== 8'hCC) begin
            $display("FAILED: gen_named.inner.val=%h expected CC",
                     gen_named.inner.val);
            $finish;
        end

        // Check nested if-generate with port access from generate body
        mux_a = 1; mux_b = 0;
        #1;
        if (mux_y !== 1'b1) begin
            $display("FAILED: mux_y=%b expected 1 (sel=0, a=1)", mux_y);
            $finish;
        end
        mux_a = 0;
        #1;
        if (mux_y !== 1'b0) begin
            $display("FAILED: mux_y=%b expected 0 (sel=0, a=0)", mux_y);
            $finish;
        end

        $display("PASSED");
        $finish;
    end
endmodule
