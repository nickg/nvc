set -xe

pwd
which nvc

# Negative: task-local variable addressed from outside via hier path.
# IEEE 1364 section 12.4.3: variables declared inside a static task are
# scoped to that task and must not be accessible by hierarchical path
# from outside the task's invocation scope.
#
# A clean diagnostic is expected (not an ICE or silent wrong-value).

cat >vlog86.v <<'EOF'
module inner;
    task my_task;
        reg [3:0] loc;
        begin
            loc = 4'd5;
        end
    endtask
endmodule

module vlog86;
    inner u ();

    reg [3:0] r;
    initial begin
        r = u.my_task.loc;
        $display("r=%0d", r);
        $finish;
    end
endmodule
EOF

nvc -a vlog86.v

# Elaboration (or analysis) must fail with a diagnostic about the
# task-local variable not being accessible.
! nvc -e vlog86 2>err

# Grep for a diagnostic mentioning the inaccessible reference.
# Accept either "task" scope / "not accessible" / "not visible" phrasing.
grep -i -E "task|not accessible|not visible|no visible declaration" err
