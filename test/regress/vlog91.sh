set -xe

pwd
which nvc

# Negative: named gate instance scope — pin the decision as
# "not addressable, diagnose".
# IEEE 1364 section 7.1 allows named gate instances (e.g. and g1(y,a,b))
# but the LRM is ambiguous about whether gate instance names introduce
# an addressable scope.  We choose: NOT addressable.  Attempting to
# reference through a named gate instance must produce a diagnostic.

cat >vlog91.v <<'EOF'
module inner;
    wire y, a, b;
    assign a = 1'b0;
    assign b = 1'b1;
    and g1 (y, a, b);
endmodule

module vlog91;
    inner u ();

    reg r;
    initial begin
        r = u.g1.y;
        $display("r=%b", r);
        $finish;
    end
endmodule
EOF

nvc -a vlog91.v

# Must fail: gate instance names are not addressable scopes.
! nvc -e vlog91 2>err

# Accept diagnostic about gate / not a scope / not addressable /
# no visible declaration.
grep -i -E "gate|scope|not addressable|no visible declaration|cannot" err
