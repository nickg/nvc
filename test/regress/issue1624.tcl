set root [vhpi::handle RootInst 0]
set data [vhpi::handle_by_name "data_in.data" $root]
set lane [vhpi::handle_by_index IndexedNames $data 0]
set valid [vhpi::handle_by_name "data_in.valid" $root]

vhpi::put_value $lane "00000000000000000000000000000000" DepositPropagate
run 1 ns
vhpi::assert {[vhpi::get_value $lane] == "00000000000000000000000000000000"}

vhpi::put_value $valid 1 DepositPropagate
run 1 ns
vhpi::assert {[vhpi::get_value $valid] == "1"}
