set root [vhpi::handle RootInst 0]
vhpi::assert {[vhpi::get_str KindStr $root] == "vhpiRootInstK"}

set t1 [vhpi::handle_by_name "t1" $root]
vhpi::assert {[vhpi::get_str KindStr $t1] == "vhpiArrayTypeDeclK"}
vhpi::assert {[vhpi::get NumDimensions $t1] == 1}
vhpi::assert [vhpi::get IsUnconstrained $t1]

set s1 [vhpi::handle_by_name "s1" $root]
vhpi::assert {[vhpi::get_str KindStr $s1] == "vhpiSigDeclK"}

set s1_type [vhpi::handle Type $s1]
vhpi::assert {[vhpi::get_str KindStr $s1_type] == "vhpiSubtypeDeclK"}
vhpi::assert {![vhpi::get IsUnconstrained $s1_type]}
vhpi::assert {[vhpi::compare_handles [vhpi::handle BaseType $s1] $t1]}
vhpi::assert {[vhpi::compare_handles [vhpi::handle BaseType $s1_type] $t1]}
vhpi::assert {[vhpi::get Size $s1_type] == 3}

set s2 [vhpi::handle_by_name "s2" $root]
vhpi::assert {[vhpi::get_str KindStr $s2] == "vhpiSigDeclK"}

set s2_type [vhpi::handle Type $s2]
vhpi::assert {[vhpi::get_str KindStr $s2_type] == "vhpiIntTypeDeclK"}

puts "S2=[vhpi::get_value $s2]"
vhpi::assert {[vhpi::get_value $s2] == 42}

set s3_x [vhpi::handle_by_name ":vhpi20:s3.x" 0]
vhpi::assert {[vhpi::get_str KindStr $s3_x] == "vhpiSelectedNameK"}

puts "S3.X=[vhpi::get_value $s3_x]"
vhpi::assert {[vhpi::get_value $s3_x] == 55}

set s3_y [vhpi::handle_by_name "s3.y" $root]
vhpi::assert {[vhpi::get_str KindStr $s3_y] == "vhpiSelectedNameK"}

puts "S3.Y=[vhpi::get_value $s3_y]"
vhpi::assert {[vhpi::get_value $s3_y] == -77}

set s3_z [vhpi::handle_by_name "s3.z" $root]
vhpi::assert {[vhpi::get_str KindStr $s3_z] == "vhpiSelectedNameK"}

for {set i 0} {$i < 3} {incr i} {
  set s3_z_i [vhpi::handle_by_index IndexedNames $s3_z $i]
  vhpi::assert {[vhpi::get_str KindStr $s3_z_i] == "vhpiIndexedNameK"}
  puts "S3.Z($i)=[vhpi::get_value $s3_z_i]"
  vhpi::assert {[vhpi::get_value $s3_z_i] == [expr $i + 1]}
}

set t3 [vhpi::handle_by_name "t3" $root]
vhpi::assert {[vhpi::get_str KindStr $t3] == "vhpiArrayTypeDeclK"}
vhpi::assert {[vhpi::get NumDimensions $t3] == 1}
vhpi::assert [vhpi::get IsUnconstrained $t3]

set s4 [vhpi::handle_by_name "s4" $root]
vhpi::assert {[vhpi::get_str KindStr $s4] == "vhpiSigDeclK"}

set b1 [vhpi::handle_by_name "b1" $root]
vhpi::assert {[vhpi::get_str KindStr $b1] == "vhpiBlockStmtK"}

set g1 [vhpi::handle_by_name "g1" $b1]
vhpi::assert {[vhpi::get_str KindStr $g1] == "vhpiGenericDeclK"}

set g1_1 [vhpi::handle_by_index IndexedNames $g1 1]
vhpi::assert {[vhpi::get_str KindStr $g1_1] == "vhpiIndexedNameK"}

set g1_1_x [vhpi::handle_by_name "x" $g1_1]
vhpi::assert {[vhpi::get_str KindStr $g1_1_x] == "vhpiSelectedNameK"}
puts "G1(1).X=[vhpi::get_value $g1_1_x]"
vhpi::assert {[vhpi::get_value $g1_1_x] == "1"}

set s4_type [vhpi::handle Type $s4]
vhpi::assert {[vhpi::get_str KindStr $s4_type] == "vhpiSubtypeDeclK"}
vhpi::assert {![vhpi::get IsUnconstrained $s4_type]}
vhpi::assert {[vhpi::compare_handles [vhpi::handle BaseType $s4] $t3]}
vhpi::assert {[vhpi::compare_handles [vhpi::handle BaseType $s4_type] $t3]}
vhpi::assert {[vhpi::get Size $s4_type] == 4}

set s4_type_c0 [vhpi::handle_by_index Constraints $s4_type 0]
vhpi::assert {[vhpi::get_str KindStr $s4_type_c0] == "vhpiIntRangeK"}
vhpi::assert {[vhpi::get LeftBound $s4_type_c0] == 1}
vhpi::assert {[vhpi::get RightBound $s4_type_c0] == 2}
vhpi::assert {[vhpi::get IsUp $s4_type_c0]}

set s4_outer_indices {1 2}
set s4_inner_indices {7 6}
set s4_expected {{1 2} {3 4}}

for {set i 0} {$i < 2} {incr i} {
  set s4_i [vhpi::handle_by_index IndexedNames $s4 $i]
  vhpi::assert {[vhpi::get_str KindStr $s4_i] == "vhpiIndexedNameK"}

  set s4_i_type [vhpi::handle Type $s4_i]
  vhpi::assert {[vhpi::get_str KindStr $s4_i_type] == "vhpiSubtypeDeclK"}
  vhpi::assert {[vhpi::get Size $s4_i_type] == 2}

  set s4_i_c0 [vhpi::handle_by_index Constraints $s4_i_type 0]
  vhpi::assert {[vhpi::get_str KindStr $s4_i_c0] == "vhpiIntRangeK"}
  vhpi::assert {[vhpi::get LeftBound $s4_i_c0] == 7}
  vhpi::assert {[vhpi::get RightBound $s4_i_c0] == 6}
  vhpi::assert {![vhpi::get IsUp $s4_i_c0]}

  set outer_index [lindex $s4_outer_indices $i]
  set row_expected [lindex $s4_expected $i]

  for {set j 0} {$j < 2} {incr j} {
    set s4_i_j [vhpi::handle_by_index IndexedNames $s4_i $j]
    vhpi::assert {[vhpi::get_str KindStr $s4_i_j] == "vhpiIndexedNameK"}

    set inner_index [lindex $s4_inner_indices $j]
    set expected [lindex $row_expected $j]
    puts "S4($outer_index,$inner_index)=[vhpi::get_value $s4_i_j]"
    vhpi::assert {[vhpi::get_value $s4_i_j] == $expected}
  }
}
