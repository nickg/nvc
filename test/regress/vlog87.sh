set -xe

pwd
which nvc

# Negative: automatic task local not addressable by hier path.
# IEEE 1364 section 10.3.1 / IEEE 1800 section 13.5.2: variables in an
# automatic task are per-invocation and must never be reachable by
# hierarchical path, even by name.  This is a stronger prohibition than
# for static tasks (vlog86): a static task's locals ARE addressable
# while the task is active, but an automatic task's locals are NEVER
# addressable.
#
# The diagnostic must be distinct from the static-task case (vlog86),
# ideally mentioning "automatic".

cat >vlog87.v <<'EOF'
module inner;
    task automatic my_task;
        reg [3:0] loc;
        begin
            loc = 4'd7;
        end
    endtask
endmodule

module vlog87;
    inner u ();

    reg [3:0] r;
    initial begin
        r = u.my_task.loc;
        $display("r=%0d", r);
        $finish;
    end
endmodule
EOF

nvc -a vlog87.v

# Must fail with a diagnostic.
! nvc -e vlog87 2>err

# Ideally mentions "automatic"; fall back to general inaccessibility.
grep -i -E "automatic|not accessible|not visible|no visible declaration" err
