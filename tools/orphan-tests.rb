#!/usr/bin/env ruby

#
# Find any tests in test/regress that are not in testlist.txt
#

require 'rubygems'
require 'pathname'

ScriptDir = Pathname.new(__FILE__).realpath.dirname
TestDir   = ScriptDir + "../test/regress"

have = File.open(TestDir + "testlist.txt").each_line.collect do |l|
  l.split(/ /)[0]
end

files = Dir.glob(TestDir + "*.vhd").collect do |f|
  name = File.basename(f).gsub(/\.vhd/, '')
  puts "missing #{name}" unless have.any? { |s| s == name }
end
