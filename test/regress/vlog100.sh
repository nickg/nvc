set -xe

pwd
which nvc

# NOTE: this test is currently blocked on an orthogonal parser gap —
# nvc does not accept `event` declarations inside tasks today.  When
# that parser support lands, this test exercises the hier-ref negative
# rule for task-local named events.  Until then, `nvc -a` fails with
# "unexpected event" rather than reaching the elaboration diagnostic
# the test pins.  Keep as a spec.
#
# Negative: named event declared inside a task is task-local.
# IEEE 1364-2005 §12.4.3: task locals are not accessible by hierarchical
# path from outside the task's invocation scope.  The rule applies
# uniformly across decl kinds — `event` inside a task is no different
# from `reg` inside a task.
#
# Consuming `u.tsk.ev` from outside (via `->`, `@`, or
# `wait(...triggered)`) must diagnose, not silently resolve.

cat >vlog100.v <<'EOF'
module inner;
    task my_task;
        event ev;
        begin
            -> ev;
        end
    endtask
endmodule

module vlog100;
    inner u ();

    initial begin
        // Attempt to trigger a task-local event from outside: must fail.
        -> u.my_task.ev;
        $display("SHOULD NOT REACH");
        $finish;
    end
endmodule
EOF

nvc -a vlog100.v

! nvc -e vlog100 2>err

grep -i -E "task|not accessible|not visible|no visible declaration" err
