set root [vhpi::handle RootInst 0]

set g_str [vhpi::handle_by_name "g_str" $root]
vhpi::assert {[vhpi::get_value $g_str] == "hello"}

set c_str [vhpi::handle_by_name "c_str" $root]
vhpi::assert {[vhpi::get_value $c_str] == "world"}

set c_real [vhpi::handle_by_name "c_real" $root]
vhpi::assert {[vhpi::get_value $c_real] == 1.5}

set s_real [vhpi::handle_by_name "s_real" $root]
vhpi::assert {[vhpi::get_value $s_real] == -0.25}

run 1 ns
vhpi::assert {[vhpi::get_value $s_real] == -0.25}
