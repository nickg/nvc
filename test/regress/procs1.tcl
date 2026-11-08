proc expect {script expected} {
    set actual [uplevel 1 $script]
    if {$actual ne $expected} {
        return -code error "$script returned '$actual' (expected '$expected')"
    }
}

# The file name depends on how the test was run
proc nofile {json} {
    return [regsub -all {"file":"[^"]*"} $json {"file":"F"}]
}

proc states {} {
    set result {}
    foreach p [processes] {
        lappend result [dict get $p path] [dict get $p state]
    }
    return $result
}

# Processes are scheduled to run once at time zero
expect {states} {/driver ready /uut/follow ready}

run 5 ns
expect {states} {/driver delayed /uut/follow waiting}

expect {nofile [processes -json]} \
    {{"v":1,"processes":[{"name":"driver","path":"/driver","state":"delayed","file":"F","line":25},{"name":"follow","path":"/uut/follow","state":"waiting","file":"F","line":9}]}}

# Optional glob pattern on the full path
expect {nofile [processes -json /uut/*]} \
    {{"v":1,"processes":[{"name":"follow","path":"/uut/follow","state":"waiting","file":"F","line":9}]}}
expect {processes -json /nothing*} {{"v":1,"processes":[]}}
expect {llength [processes /nothing*]} 0

set d [lindex [processes /driver] 0]
expect {dict get $d name} driver
expect {dict get $d line} 25

run
expect {states} {/driver waiting /uut/follow waiting}

# Bad usage
expect {catch {processes -bogus}} 1
expect {catch {processes a b}} 1
