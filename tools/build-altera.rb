#!/usr/bin/env ruby

#
# Script to compile the Altera simulation libraries
#
# See the following page for the library mappings:
#    http://quartushelp.altera.com/current/mergedProjects/eda/eda_topics/
#       quartus2/eda_ref_presynth_lib.htm
#

require 'fileutils'

SearchRoot = '/opt/altera'

quartus = nil
Dir.new(SearchRoot).each do |e|
  next if e =~ /^\./
  q = "#{SearchRoot}/#{e}/quartus"
  quartus = q if Dir.exists? "#{q}/eda/sim_lib"
end

unless quartus
  puts "Cannot find Quartus install under #{SearchRoot}"
  exit 1
end

puts "Using Quartus installation in #{quartus}"

$src = "#{quartus}/eda/sim_lib"

$libdir = "#{File.expand_path '~'}/.nvc/lib"
FileUtils.mkdir_p $libdir

def run_nvc(lib, file)
  cmd = "nvc --work=#{$libdir}/#{lib} -a --prefer-explicit #{$src}/#{file}"
  puts cmd
  exit 1 unless system cmd
end

def run_codegen(lib, unit)
  cmd = "nvc --work=#{$libdir}/#{lib} --codegen #{unit}"
  puts cmd
  exit 1 unless system "#{cmd} > /dev/null"
end

def put_title(what)
  puts
  puts "------ #{what} ------"
end

put_title 'ALTERA library'
run_nvc 'altera', 'altera_primitives_components.vhd'
run_nvc 'altera', 'altera_primitives.vhd'

put_title 'ALTERA_MF library'
run_nvc 'altera_mf', 'altera_mf_components.vhd'
run_nvc 'altera_mf', 'altera_mf.vhd'

put_title 'LPM library'
run_nvc 'lpm', '220pack.vhd'
run_nvc 'lpm', '220model.vhd'

put_title 'ALTERA_LNSIM library'
run_nvc 'altera_lnsim', 'altera_lnsim_components.vhd'

put_title 'SGATE library'
run_nvc 'sgate', 'sgate_pack.vhd'
run_nvc 'sgate', 'sgate.vhd'

put_title 'ARRIAII library'
run_nvc 'arriaii', 'arriaii_atoms.vhd'
run_nvc 'arriaii', 'arriaii_components.vhd'
run_nvc 'arriaii', 'arriaii_hssi_components.vhd'
# Broken, at least in 13.0sp1
#run_nvc 'arriaii', 'arriaii_hssi_atoms.vhd'

put_title 'ARRIAV library'
run_nvc 'arriav', 'arriav_atoms.vhd'
run_nvc 'arriav', 'arriav_components.vhd'
run_nvc 'arriav', 'arriav_hssi_components.vhd'
run_nvc 'arriav', 'arriav_hssi_atoms.vhd'

put_title 'ARRIAVGZ library'
run_nvc 'arriavgz', 'arriavgz_atoms.vhd'
run_nvc 'arriavgz', 'arriavgz_components.vhd'
run_nvc 'arriavgz', 'arriavgz_hssi_components.vhd'
run_nvc 'arriavgz', 'arriavgz_hssi_atoms.vhd'
run_nvc 'arriavgz', 'arriavgz_pcie_hip_atoms.vhd'
run_nvc 'arriavgz', 'arriavgz_pcie_hip_components.vhd'

put_title 'CYCLONEIII library'
run_nvc 'cycloneiii', 'cycloneiii_atoms.vhd'
run_nvc 'cycloneiii', 'cycloneiii_components.vhd'

put_title 'CYCLONEIV library'
run_nvc 'cycloneiv', 'cycloneiv_atoms.vhd'
run_nvc 'cycloneiv', 'cycloneiv_components.vhd'
run_nvc 'cycloneiv', 'cycloneiv_hssi_components.vhd'
# Broken, at least in 13.0sp1
#run_nvc 'cycloneiv', 'cycloneiv_hssi_atoms.vhd'
run_nvc 'cycloneiv_pcie_hip', 'cycloneiv_pcie_hip_components.vhd'
run_nvc 'cycloneiv_pcie_hip', 'cycloneiv_pcie_hip_atoms.vhd'
run_nvc 'cycloneiv', 'cycloneive_atoms.vhd'
run_nvc 'cycloneiv', 'cycloneive_components.vhd'

put_title 'CYCLONEV library'
run_nvc 'cyclonev', 'cyclonev_atoms.vhd'
run_nvc 'cyclonev', 'cyclonev_components.vhd'

put_title 'MAX library'
run_nvc 'max', 'max_atoms.vhd'
run_nvc 'max', 'max_components.vhd'

put_title 'MAXII library'
run_nvc 'maxii', 'maxii_atoms.vhd'
run_nvc 'maxii', 'maxii_components.vhd'

put_title 'MAXV library'
run_nvc 'maxv', 'maxv_atoms.vhd'
run_nvc 'maxv', 'maxv_components.vhd'

put_title 'STRATIXIII library'
run_nvc 'stratixiii', 'stratixiii_atoms.vhd'
run_nvc 'stratixiii', 'stratixiii_components.vhd'

put_title 'STRATIXV library'
run_nvc 'stratixv', 'stratixv_atoms.vhd'
run_nvc 'stratixv', 'stratixv_components.vhd'
run_nvc 'stratixv_hssi', 'stratixv_hssi_components.vhd'
run_nvc 'stratixv_hssi', 'stratixv_hssi_atoms.vhd'
run_nvc 'stratixv_pcie_hip', 'stratixv_pcie_hip_components.vhd'
run_nvc 'stratixv_pcie_hip', 'stratixv_pcie_hip_atoms.vhd'
