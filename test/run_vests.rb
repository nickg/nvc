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

def run_cmd(c)
  Process.wait Process.spawn(c)
  puts "#{c} aborted" unless $?.exitstatus < 2
  $?.exitstatus == 0
end

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

  cmd = "#{Tool} -a #{Billowitch}/#{item}"
  if run_cmd cmd then
    # TODO: elaborate, run
    passes += 1
    print '+'.green
  else
    puts cmd.magenta
    fails += 1
  end
end

puts
puts "#{passes} passes"
puts "#{fails} failures"

File.open("#{VestsDir}/HISTORY", 'a') do |f|
  f.printf("%20s %10s   %4d passes   %4d failures\n",
    Time.new.ctime, GitRev, passes, fails)
end
