proc check_word {object expected} {
  vhpi::assert {[vhpi::get_str KindStr $object] == "vhpiConstDeclK"}

  set type [vhpi::handle Type $object]
  vhpi::assert {[vhpi::get_str KindStr $type] == "vhpiSubtypeDeclK"}
  vhpi::assert {![vhpi::get IsUnconstrained $type]}

  set constraint [vhpi::handle_by_index Constraints $type 0]
  vhpi::assert {[vhpi::get_str KindStr $constraint] == "vhpiIntRangeK"}
  vhpi::assert {![vhpi::get IsUnconstrained $constraint]}
  vhpi::assert {[vhpi::get LeftBound $constraint] == 0}
  vhpi::assert {[vhpi::get RightBound $constraint] == 3}
  vhpi::assert {[vhpi::get IsUp $constraint]}
  vhpi::assert {[vhpi::get Size $type] == 4}

  for {set i 0} {$i < 4} {incr i} {
    set element [vhpi::handle_by_index IndexedNames $object $i]
    vhpi::assert {[vhpi::get_str KindStr $element] == "vhpiIndexedNameK"}
    vhpi::assert {[vhpi::get_value $element] == [lindex $expected $i]}
  }
}

set root [vhpi::handle RootInst 0]
vhpi::assert {[vhpi::get_str KindStr $root] == "vhpiRootInstK"}

set expected {10 20 30 40}

# These constants obtain their constraints from a function call.
check_word [vhpi::handle_by_name "c_from_func" $root] $expected
check_word [vhpi::handle_by_name "c_tmp" $root] $expected
check_word [vhpi::handle_by_name "c_forwarded" $root] $expected

# An equivalent aggregate-initialised constant is the working control case.
check_word [vhpi::handle_by_name "c_from_list" $root] $expected

run 1 ns

set word_o [vhpi::handle_by_name "word_o" $root]
vhpi::assert {[vhpi::get_str KindStr $word_o] == "vhpiPortDeclK"}
for {set i 0} {$i < 4} {incr i} {
  set element [vhpi::handle_by_index IndexedNames $word_o $i]
  vhpi::assert {[vhpi::get_value $element] == [lindex $expected $i]}
}

set matrix_o [vhpi::handle_by_name "matrix_o" $root]
vhpi::assert {[vhpi::get_str KindStr $matrix_o] == "vhpiPortDeclK"}
for {set i 0} {$i < 4} {incr i} {
  set word [vhpi::handle_by_index IndexedNames $matrix_o $i]
  for {set j 0} {$j < 4} {incr j} {
    set element [vhpi::handle_by_index IndexedNames $word $j]
    vhpi::assert {[vhpi::get_value $element] == [lindex $expected $j]}
  }
}
