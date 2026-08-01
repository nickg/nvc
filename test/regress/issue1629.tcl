set root [vhpi::handle RootInst 0]
set s [vhpi::handle_by_name "s" $root]

vhpi::put_value $s 1 ForcePropagate
run 1 ns
vhpi::assert {[vhpi::get_value $s] == "1"}

vhpi::put_value $s 0 Release
vhpi::put_value $s 1 ForcePropagate
run 1 ns
vhpi::assert {[vhpi::get_value $s] == "1"}
