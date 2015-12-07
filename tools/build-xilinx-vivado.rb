#!/usr/bin/env ruby

#
# Script to compile the Xilinx Vivado simulation libraries
#

require 'fileutils'

xilinx = ENV['XILINX_VIVADO']

unless xilinx
  # Try searching some common installation directories
  search = (2014..2015).collect do |major|
    (1..4).collect { |minor| [
        "/opt/Xilinx/Vivado/#{major}.#{minor}",
        "/cygdrive/C/Xilinx/Vivado/#{major}.#{minor}" ] }
  end.flatten.reverse

  unless search.any? { |root| File.directory?(xilinx = root) }
    raise "No Vivado installation found: set XILINX_VIVADO environment variable"
  end
end

$src = "#{xilinx}/data/vhdl/src"

unless File.directory? $src
  raise "Source directory #{$src} does not exist"
end

puts "Using Vivado installation in #{xilinx}"

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
run_nvc "unisim", "unisims/unisim_retarget_VCOMP.vhd"
run_codegen "unisim", "vpkg"
run_codegen "unisim", "vcomponents"

put_title "UNIMACRO library"
run_nvc "unimacro", "unimacro/unimacro_VCOMP.vhd"
Dir.glob "#{$src}/unimacro/*_MACRO.vhd" do |f|
  run_nvc "unimacro", f
end

put_title "Primitives"

# vhdl_analyze_order lists a file that doesn't exist
unisim_skip = [
  'FF_LATCH.vhd',
]

unisim_order = "#{$src}/unisims/primitive/vhdl_analyze_order"
File.open(unisim_order).each_line do |line|
  line.chomp!
  unless line =~ /^ *$/ or unisim_skip.include? line then
    run_nvc "unisim", "unisims/primitive/#{line}"
  end
end

put_title "UNIFAST library"

unifast_order = "#{$src}/unifast/primitive/vhdl_analyze_order"
File.open(unifast_order).each_line do |line|
  line.chomp!
  run_nvc "unifast", "unifast/primitive/#{line}"
end

put_title "Finished"
puts "Xilinx Vivado libraries installed in #{$libdir}"
