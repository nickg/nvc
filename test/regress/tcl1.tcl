proc assert condition {
    if {![uplevel 1 $condition]} {
        return -code error "assertion failed: $condition"
    }
}

set testdir [file dirname [file normalize [info script]]]

puts testdir

vcom $testdir/signal1.vhd
vsim signal1

puts "$now +$deltas"
assert {expr $now == 0}
assert {expr $deltas == 0}

run

puts "$now +$deltas"
assert {expr $now == 1000000}
assert {expr $deltas == 1}

set sigs [find signals /*]
puts $sigs
assert {expr [llength $sigs] == 1}
assert {string equal [lindex $sigs 0] "/x"}
