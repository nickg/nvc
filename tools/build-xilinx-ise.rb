#!/usr/bin/env ruby

#
# Script to compile the Xilinx simulation libraries
#

require 'fileutils'

xilinx = ENV['XILINX']

unless xilinx
  # Try searching some common installation directories
  search = (10..14).collect do |major|
    (1..7).collect { |minor| [
        "/opt/Xilinx/#{major}.#{minor}/ISE_DS/ISE",
        "/cygdrive/C/Xilinx/#{major}.#{minor}/ISE_DS/ISE" ] }
  end.flatten.reverse

  unless search.any? { |root| File.directory?(xilinx = root) }
    raise "No ISE installation found: set XILINX environment variable"
  end
end

$src = "#{xilinx}/vhdl/src"

unless File.directory? $src
  raise "Source directory #{$src} does not exist"
end

puts "Using ISE installation in #{xilinx}"

# ISE has conflicting version of libstdc++ on some Linux systems
ENV['LD_LIBRARY_PATH'] = nil

$libdir = "#{File.expand_path '~'}/.nvc/lib"
FileUtils.mkdir_p $libdir

def run_nvc(lib, file)
  file = "#{$src}/#{file}" unless file =~ /^\//
  cmd = "nvc --work=#{$libdir}/#{lib} -a --relax=prefer-explicit,pure-files #{file}"
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

put_title "UNISIM package"
run_nvc "unisim", "unisims/unisim_VPKG.vhd"
run_nvc "unisim", "unisims/unisim_VCOMP.vhd"
run_codegen "unisim", "vpkg"
run_codegen "unisim", "vcomponents"

put_title "UNIMACRO library"
run_nvc "unimacro", "unimacro/unimacro_VCOMP.vhd"
Dir.glob "#{$src}/unimacro/*_MACRO.vhd" do |f|
  run_nvc "unimacro", f
end

put_title "Primitives"

unisim_order = "#{$src}/unisims/primitive/vhdl_analyze_order"
File.open(unisim_order).each_line do |line|
  line.chomp!
  run_nvc "unisim", "unisims/primitive/#{line}"
end

put_title "XilinxCoreLib"

# Some files in XilinxCoreLib cannot be built
xcl_skip = [
  # No configurations
  'prims_sim_arch_v4_0.vhd',

  # Slice out of bounds
  'fir_compiler_v5_1_sim_pkg.vhd',
  'fir_compiler_v5_0_sim_pkg.vhd',
  'cic_compiler_v3_0_fir_pkg.vhd',

  # Dependency on one of the above
  'mult_gen_v10_0.vhd',
  'fir_compiler_v5_1.vhd',
  'fir_compiler_v5_1_mac_fir.vhd',
  'fir_compiler_v5_0.vhd',
  'fir_compiler_v5_0_mac_fir.vhd',
  'fir_compiler_v5_0_da_fir.vhd',
  'cic_compiler_v3_0_old_pkg.vhd',
  'cic_compiler_v3_0_old_sim_comps.vhd',
  'cic_compiler_v3_0_old.vhd',
  'cic_compiler_v3_0_old_comp.vhd',
  'cic_compiler_v3_0_old_xst.vhd',
  'cic_compiler_v3_0_old_xst_comp.vhd',
  'cic_compiler_v3_0_pkg.vhd',
  'cic_compiler_v3_0_decimate_bhv.vhd',
  'cic_compiler_v3_0_old_interpolate_bhv.vhd',
  'cic_compiler_v3_0_old_decimate_bhv.vhd',
  'cic_compiler_v3_0_sim_comps.vhd',
  'cic_compiler_v3_0.vhd',
  'cic_compiler_v3_0_comp.vhd',
  'cic_compiler_v3_0_xst.vhd',
  'cic_compiler_v3_0_xst_comp.vhd',
  'cic_compiler_v3_0_interpolate_bhv.vhd',
  'cic_compiler_v2_0_pkg.vhd',
  'cic_compiler_v2_0_interpolate_bhv.vhd',
  'cic_compiler_v2_0_decimate_bhv.vhd',
  'cic_compiler_v2_0_sim_comps.vhd',
  'cic_compiler_v2_0.vhd',
  'cic_compiler_v2_0_comp.vhd',
  'cic_compiler_v2_0_xst.vhd',
  'cic_compiler_v2_0_xst_comp.vhd',
  'cic_compiler_v1_3_pkg.vhd',
  'cic_compiler_v1_3_interpolate_bhv.vhd',
  'cic_compiler_v1_3_decimate_bhv.vhd',
  'cic_compiler_v1_3_sim_comps.vhd',
  'cic_compiler_v1_3.vhd',
  'cic_compiler_v1_3_comp.vhd',
  'cic_compiler_v1_3_xst.vhd',
  'cic_compiler_v1_3_xst_comp.vhd',
]

corelib_order = "#{$src}/XilinxCoreLib/vhdl_analyze_order"
File.open(corelib_order).each_line do |line|
  line.gsub!(/#.*$/, '')
  line.chomp!
  unless line =~ /^ *$/ or xcl_skip.include? line then
    run_nvc "xilinxcorelib", "XilinxCoreLib/#{line}"
  end
end

put_title "Finished"
puts "Xilinx libraries installed in #{$libdir}"
