set root [vhpi::handle RootInst 0]
set clk [vhpi::handle_by_name "clk" $root]
set count [vhpi::handle_by_name "count" $root]

vhpi::put_value $clk 0 DepositPropagate
vhpi::put_value $clk 0 DepositPropagate
run 1 ns
vhpi::assert {[vhpi::get_value $clk] == "0"}

for {set i 1} {$i <= 5} {incr i} {
    vhpi::put_value $clk 1 DepositPropagate
    vhpi::put_value $clk 1 DepositPropagate
    run 1 ns
    vhpi::assert {[vhpi::get_value $clk] == "1"}
    vhpi::assert {[vhpi::get_value $count] == $i}

    vhpi::put_value $clk 0 DepositPropagate
    vhpi::put_value $clk 0 DepositPropagate
    run 1 ns
    vhpi::assert {[vhpi::get_value $clk] == "0"}
}
