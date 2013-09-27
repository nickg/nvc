#!/usr/bin/env ruby

#
# Script to compile the Xilinx simulation libraries
#

require 'fileutils'

xilinx = ENV['XILINX']

unless xilinx
  # Try searching some common installation directories
  search = (10..14).collect do |major|
    (1..4).collect { |minor| "/opt/Xilinx/#{major}.#{minor}/ISE_DS/ISE" }
  end.flatten.reverse

  unless search.any? { |root| Dir.exists? (xilinx = root) }
    die "No ISE installation found: set XILINX environment variable"
  end
end

$src = "#{xilinx}/vhdl/src"

unless Dir.exists? $src
  die "Source directory #{$src} does not exist"
end

puts "Using ISE installation in #{xilinx}"

# ISE has conflicting version of libstdc++ on some Linux systems
ENV['LD_LIBRARY_PATH'] = nil

$libdir = "#{File.expand_path '~'}/.nvc/lib"
FileUtils.mkdir_p $libdir

def run_nvc(lib, file)
  cmd = "nvc --work=#{$libdir}/#{lib} -a #{$src}/#{file}"
  puts cmd
  exit 1 unless system cmd
end

def put_title(what)
  puts
  puts "------ #{what} ------"
end

put_title "UNISIM package"
run_nvc "unisim", "unisims/unisim_VPKG.vhd"
run_nvc "unisim", "unisims/unisim_VCOMP.vhd"

put_title "Primitives"

unisim_order = "#{$src}/unisims/primitive/vhdl_analyze_order"
File.open(unisim_order).each_line do |line|
  run_nvc "unisim", "unisims/primitive/#{line}"
end

# XilinxCoreLib does not yet build
#
#put_title "XilinxCoreLib"
#
#corelib_order = "#{$src}/XilinxCoreLib/vhdl_analyze_order"
#File.open(corelib_order).each_line do |line|
#  line.gsub!(/#.*$/, '')
#  unless line =~ /^ *$/ then
#    run_nvc "xilinxcorelib", "XilinxCoreLib/#{line}"
#  end
#end

put_title "Finished"
puts "Xilinx libraries installed in #{$libdir}"
