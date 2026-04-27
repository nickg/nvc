set -xe

pwd
which nvc

# Negative: defparam ordering violation (IEEE 1364 section 12.2.1).
# A defparam whose target lies inside a generate block whose condition
# depends on another defparam is illegal.  This must produce a
# diagnostic rather than an ICE or silently choosing the wrong value.

cat >vlog90.v <<'EOF'
module inner;
    parameter P = 0;

    generate
        if (P == 1) begin : gen_true
            parameter Q = 0;
            wire [3:0] w = Q;
        end
    endgenerate
endmodule

module vlog90;
    inner u ();

    // First defparam controls whether gen_true exists.
    defparam u.P = 1;

    // Second defparam targets something inside the conditional generate
    // whose existence depends on the first defparam — this is an
    // illegal ordering per section 12.2.1.
    defparam u.gen_true.Q = 42;

    initial begin
        $display("should not reach here");
        $finish;
    end
endmodule
EOF

nvc -a vlog90.v

# Must fail with a diagnostic about defparam ordering / cannot resolve
# the target path / generate dependency.
! nvc -e vlog90 2>err

# Accept diagnostic about defparam, ordering, generate, or unresolved path.
grep -i -E "defparam|order|generate|no visible|cannot resolve|not found" err
