#!/usr/bin/env ruby

#
# Script to compile the Lattice simulation libraries
#

require 'fileutils'

SearchRoot = ARGV[0] || '/opt/lscc'

icecube = nil
Dir.new(SearchRoot).each do |e|
  next if e =~ /^\./
  next unless e =~ /^iCEcube2/
  i = "#{SearchRoot}/#{e}"
  icecube = i if Dir.exists? "#{i}/vhdl"
end

unless icecube
  puts "Cannot find iCEcube2 install under #{SearchRoot}"
  puts "Try passing the installation directory as an argument"
  exit 1
end

puts "Using iCEcube2 installation in #{icecube}"

$src = "#{icecube}/vhdl"

$libdir = "#{File.expand_path '~'}/.nvc/lib"
FileUtils.rm_r $libdir
FileUtils.mkdir_p $libdir

def run_nvc(lib, file)
  cmd = "nvc --work=#{$libdir}/#{lib} -a --relax=prefer-explicit #{$src}/#{file}"
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

put_title 'ICE library'
run_nvc 'ice', 'vcomponent_vital.vhd'
run_nvc 'ice', 'sb_ice_syn_vital.vhd'
run_nvc 'ice', 'sb_ice_lc_vital.vhd'
