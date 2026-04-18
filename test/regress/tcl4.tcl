set null [vhpi::null]
puts $null

set root [vhpi::handle RootInst $null]
puts $root

vhpi::assert {[vhpi::get_str Name $root] == "TCL4"}

set root2 [vhpi::handle_by_name "TCL4" $null]
puts $root2

vhpi::assert {[vhpi::compare_handles $root $root2]}
set du [vhpi::handle DesignUnit $root]
vhpi::assert {[vhpi::get_str KindStr $du] == "vhpiArchBodyK"}

set tool [vhpi::handle Tool $null]
vhpi::assert {[vhpi::get_str Name $tool] == "nvc"}

set sigs [vhpi::iterate SigDecls $root]
foreach s $sigs {
    puts [vhpi::get_str Name $s]
    puts [vhpi::get_value $s]
}

vhpi::assert {[llength $sigs] == 4}

set clk [vhpi::handle_by_name "clk" $root]
set dir [vhpi::handle_by_name "dir" $root]

set ctr [vhpi::handle_by_index SigDecls $root 2]
vhpi::assert {[vhpi::get_str Name $ctr] == "CTR"}

set int [vhpi::handle_by_name "int" $root]
vhpi::put_value $int 42 DepositPropagate

run 5 ns

vhpi::assert {$now == 5000000}
vhpi::assert {[vhpi::get_value $clk] == "1"}
vhpi::assert {[vhpi::get_value $ctr] == "0001"}
vhpi::assert {[vhpi::get_value $int] == "42"}
