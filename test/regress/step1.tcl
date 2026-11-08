proc expect {script expected} {
    set actual [uplevel 1 $script]
    if {$actual ne $expected} {
        return -code error "$script returned '$actual' (expected '$expected')"
    }
}

proc states {} {
    set result {}
    foreach p [processes] {
        lappend result [dict get $p path] [dict get $p state]
    }
    return $result
}

# Nothing has run yet
expect {states} {/driver ready /uut/follow ready}

# First cycle at time zero runs both processes
expect {step -json} {{"v":1,"now":0,"delta":0,"status":"paused"}}
expect {states} {/driver delayed /uut/follow waiting}
expect {examine /b} -2147483648

# Next delta cycle updates the port
expect {step} {}
expect {list $now $deltas} {0 1}
expect {examine /b} 1

# Next cycle is the first delta of the 10 ns time step
expect {step -delta -json} {{"v":1,"now":10000000,"delta":0,"status":"paused"}}
expect {examine /a} 0
expect {examine /b} 1

# Finish the time step
expect {step -time -json} {{"v":1,"now":10000000,"delta":2,"status":"paused"}}
expect {examine /b} 2

# Breakpoints
expect {breakpoint list} {}
expect {breakpoint list -json} {{"v":1,"breakpoints":[]}}
expect {breakpoint add 18 ns} 1
expect {breakpoint add -json 15 ns} {{"v":1,"id":2,"time":15000000}}
expect {breakpoint add 30 ns} 3
expect {breakpoint list} {{id 2 time 15000000} {id 1 time 18000000} {id 3 time 30000000}}
expect {breakpoint list -json} \
    {{"v":1,"breakpoints":[{"id":2,"time":15000000},{"id":1,"time":18000000},{"id":3,"time":30000000}]}}

# Cannot set a breakpoint in the past
expect {catch {breakpoint add 10 ns}} 1
expect {catch {breakpoint add 0 ns}} 1
expect {catch {breakpoint add 5 parsecs}} 1
expect {catch {breakpoint}} 1
expect {catch {breakpoint frobnicate}} 1

# No event at the breakpoint time so the simulation stays at 10 ns
expect {run -json} \
    {{"v":1,"now":10000000,"delta":2,"status":"breakpoint","breakpoint":{"id":2,"time":15000000}}}
expect {breakpoint list} {{id 1 time 18000000} {id 3 time 30000000}}

expect {run} {breakpoint 1 at 18000000 fs}
expect {expr {$now == 10000000}} 1

# The breakpoint is later than the end of the simulation
expect {run -json} {{"v":1,"now":20000000,"delta":2,"status":"finished"}}
expect {breakpoint list} {{id 3 time 30000000}}
expect {breakpoint delete 3} {}
expect {catch {breakpoint delete 3}} 1
expect {catch {breakpoint delete foo}} 1

# A user time limit that comes first is not a breakpoint
expect {breakpoint add 50 ns} 4
expect {breakpoint delete -json all} {{"v":1,"deleted":1}}
expect {breakpoint list} {}

# Stepping when there is nothing left to do
expect {step -json} {{"v":1,"now":20000000,"delta":2,"status":"finished"}}
expect {step -time -json} {{"v":1,"now":20000000,"delta":2,"status":"finished"}}
expect {catch {step -bogus}} 1
expect {catch {step 1}} 1
