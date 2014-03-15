#!/usr/bin/env ruby

require 'rubygems'
require 'pathname'
require 'colorize'
require 'timeout'
require 'fileutils'
require 'getopt/std'

TestDir = Pathname.new(__FILE__).realpath.dirname
BuildDir = Pathname.new(ENV['BUILD_DIR'] || Dir.pwd).realpath
LibPath = "#{BuildDir}/lib/std:#{BuildDir}/lib/ieee"
VestsDir = "#{TestDir}/vests"
Prefix = "#{VestsDir}/vhdl-93"
GitRev = IO::popen("git rev-parse --short HEAD").read.chomp
Tool = 'nvc'
Billowitch = "#{Prefix}/billowitch/compliant"

unless Dir.exists? VestsDir
  puts "VESTs source missing from #{VestsDir}"
  puts
  puts "You can download this using"
  puts "  git clone https://github.com/nickg/vests.git"
  exit 1
end

fails  = 0
passes = 0

#
# Billowitch, compliant
#

Dir.chdir Billowitch

Dir.foreach('.') do |item|
  next unless item =~ /\.vhdl?$/

  FileUtils.rm_rf 'work'

  cmd = "#{Tool} -a #{item}"
  if system cmd then
    # TODO: elaborate, run
    passes += 1
    print '+'.green
  else
    fails += 1
    puts "Failed #{Billowitch}/#{item}"
  end
end

puts
puts "#{passes} passes"
puts "#{fails} failures"

File.open("#{VestsDir}/HISTORY", 'a') do |f|
  f.printf("%20s %10s   %4d passes   %4d failures\n",
    Time.new.ctime, GitRev, passes, fails)
end
