proc assert condition {
    if {![uplevel 1 [list expr $condition]]} {
        return -code error "assertion failed: $condition"
    }
}

proc expect {script expected} {
    set actual [uplevel 1 $script]
    if {$actual ne $expected} {
        return -code error "$script returned '$actual' (expected '$expected')"
    }
}

# Time before any simulation has run
expect {run -json 10 ns} {{"v":1,"now":10000000,"delta":0,"status":"paused"}}
assert {$now == 10000000}

expect {examine -json /cnt} {{"v":1,"values":[{"name":"/cnt","value":"1"}]}}
expect {exa -json -hex /cnt /s} \
    {{"v":1,"values":[{"name":"/cnt","value":"0x1"},{"name":"/s","value":"\"a\"b\""}]}}

expect {find signals -json -r /*} \
    {{"v":1,"kind":"signals","names":["/clk","/cnt","/s","/uut/q","/uut/clk"]}}
expect {find regions -json -r /*} {{"v":1,"kind":"regions","names":["/uut/"]}}
expect {find signals -json /nothing*} {{"v":1,"kind":"signals","names":[]}}

expect {describe -json /s /uut/} \
    {{"v":1,"objects":[{"name":"s","path":"/s","kind":"signal","type":"STRING"},{"name":"uut","path":"/uut/","kind":"region"}]}}

# The plain text replies are unchanged
expect {examine /cnt} {1}
expect {find signals /c*} {/clk /cnt}

expect {run -json} {{"v":1,"now":40000000,"delta":0,"status":"finished"}}
