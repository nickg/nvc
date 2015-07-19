#!/usr/bin/env ruby

#
# Downloads and builds the VHDL-2008 support libraries from
#   http://www.eda.org/fphdl/
#

require 'fileutils'
require 'tmpdir'

Zip = "vhdl2008c.zip"
Url = "http://www.eda.org/fphdl/#{Zip}"
Lib = "#{File.expand_path '~'}/.nvc/lib/ieee_proposed"

def run_nvc(file)
  cmd = "nvc --std=1993 --work=#{Lib} -a #{file}"
  puts cmd
  exit 1 unless system cmd
end

Dir.mktmpdir do |dir|
  puts "Temporary directory : #{dir}"
  puts "Install directory   : #{Lib}"

  FileUtils.mkdir_p File.basename(Lib)
  Dir.chdir dir

  puts "Downloading #{Url}"
  exit 1 unless system "curl -sSL -o #{Zip} \"#{Url}\""
  exit 1 unless system "unzip #{Zip}"

  run_nvc "standard_additions_c.vhdl"
  run_nvc "standard_textio_additions_c.vhdl"
  run_nvc "env_c.vhdl"
  run_nvc "std_logic_1164_additions.vhdl"
  run_nvc "numeric_std_additions.vhdl"
  run_nvc "numeric_std_unsigned_c.vhdl"
  run_nvc "fixed_float_types_c.vhdl"
  run_nvc "fixed_pkg_c.vhdl"
end

