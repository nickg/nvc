#!/usr/bin/env ruby

#
# Script to compile the Xilinx simulation libraries
#

require 'fileutils'

xilinx = ENV['XILINX']

unless xilinx
  # Try searching some common installation directories
  search = (10..14).collect do |major|
    (1..7).collect { |minor| "/opt/Xilinx/#{major}.#{minor}/ISE_DS/ISE" }
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

put_title "UNISIM package"
run_nvc "unisim", "unisims/unisim_VPKG.vhd"
run_nvc "unisim", "unisims/unisim_VCOMP.vhd"
run_codegen "unisim", "vpkg"
run_codegen "unisim", "vcomponents"

put_title "Primitives"

unisim_order = "#{$src}/unisims/primitive/vhdl_analyze_order"
File.open(unisim_order).each_line do |line|
  line.chomp!
  run_nvc "unisim", "unisims/primitive/#{line}"
end

put_title "XilinxCoreLib"

if false

# Some files in XilinxCoreLib do not yet build
xcl_skip = [
  'prims_sim_arch_v4_0.vhd',      # No configurations
]

corelib_order = "#{$src}/XilinxCoreLib/vhdl_analyze_order"
File.open(corelib_order).each_line do |line|
  line.gsub!(/#.*$/, '')
  line.chomp!
  puts "line='#{line}' include=#{xcl_skip.include? line}"
  unless line =~ /^ *$/ or xcl_skip.include? line then
    run_nvc "xilinxcorelib", "XilinxCoreLib/#{line}"
  end
end

end

put_title "Finished"
puts "Xilinx libraries installed in #{$libdir}"
