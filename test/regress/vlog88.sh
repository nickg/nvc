set -xe

pwd
which nvc

# Negative: genvar addressed outside its generate scope.
# IEEE 1364 section 12.4.3: a genvar is scoped to its generate block
# and must not be accessible by hierarchical path from outside.

cat >vlog88.v <<'EOF'
module inner;
    genvar i;
    generate
        for (i = 0; i < 4; i = i + 1) begin : gen_blk
            wire [3:0] w = i;
        end
    endgenerate
endmodule

module vlog88;
    inner u ();

    reg [31:0] r;
    initial begin
        r = u.i;
        $display("r=%0d", r);
        $finish;
    end
endmodule
EOF

nvc -a vlog88.v

# Must fail: genvar is not accessible outside the generate scope.
! nvc -e vlog88 2>err

# Accept diagnostic mentioning genvar, generate, scope, or general
# "no visible declaration".
grep -i -E "genvar|generate|scope|not accessible|no visible declaration" err
