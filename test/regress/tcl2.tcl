proc assert condition {
    if {![uplevel 1 $condition]} {
        return -code error "assertion failed: $condition"
    }
}

set testdir [file dirname [file normalize [info script]]]

vcom $testdir/elab1.vhd
vsim elab1

set sigs [find signals /*]
puts $sigs

assert {expr [exa /x] == -2147483648}

run

assert {expr [exa /x] == 2}

restart

puts [exa /x]
assert {expr [exa /x] == -2147483648}

assert {expr $now == 0}

run

assert {expr [exa /x] == 2}
