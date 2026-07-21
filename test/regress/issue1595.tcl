set root [vhpi::handle RootInst 0]
set theta_table [vhpi::handle_by_name "theta_table" $root]

vhpi::assert {[vhpi::get_str KindStr $theta_table] == "vhpiSigDeclK"}

set theta_table_type [vhpi::handle Type $theta_table]
vhpi::assert {[vhpi::get_str KindStr $theta_table_type] == "vhpiSubtypeDeclK"}

set theta_table_0 [vhpi::handle_by_index IndexedNames $theta_table 0]
vhpi::assert {[vhpi::get_str KindStr $theta_table_0] == "vhpiIndexedNameK"}
