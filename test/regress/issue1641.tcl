set root [vhpi::handle RootInst 0]
set clk [vhpi::handle_by_name "clk" $root]
set q [vhpi::handle_by_name "q" $root]

vhpi::put_value $clk 0 DepositPropagate
run 1 ns
vhpi::put_value $clk 1 DepositPropagate
run 1 ns

vhpi::assert {[vhpi::get_value $q] == "1"}
