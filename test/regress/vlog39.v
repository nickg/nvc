// Reproducer: parameter (via `define) used as replication count
// inside a concatenation in assign. Double braces: {{count}{value}}.
// NVC fails with "expression is not constant".

`define SCALE C_SCALE

module vlog39_inner #(
    parameter integer C_SCALE = 2
)(
    output wire [`SCALE-1:0] o
);

    // Double braces: outer concat + inner replication
    assign o = {{`SCALE}{1'b0}};

endmodule

module vlog39;
    wire [1:0] o;

    vlog39_inner u (.o(o));

    initial begin
        #10;
        if (o !== 2'b00) begin
            $display("FAILED: expected 0, got %b", o);
            $finish;
        end
        $display("PASSED");
        $finish;
    end
endmodule
