set -xe

pwd
which nvc

# Negative: hier-path tail is wrong kind for the syntactic role.
# Case: a wire used as a disable target.
# IEEE 1364 section 9.6.2: disable target must name a block or task.
# A wire is neither — must produce a clean diagnostic.

cat >vlog89.v <<'EOF'
module inner;
    wire w;
endmodule

module vlog89;
    inner u ();

    initial begin
        disable u.w;
        $display("should not reach here");
        $finish;
    end
endmodule
EOF

nvc -a vlog89.v

# Must fail: wire is not a valid disable target.
! nvc -e vlog89 2>err

# Accept diagnostic about the target type / disable / not a block or task.
grep -i -E "disable|target|block|task|cannot|invalid|not a" err
