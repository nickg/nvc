#!/usr/bin/env ruby

require 'rubygems'
require 'pathname'
require 'colorize'
require 'timeout'
require 'getopt/std'

TestDir = Pathname.new(__FILE__).realpath.dirname
BuildDir = Pathname.new(ENV['BUILD_DIR'] || Dir.pwd).realpath
LibPath = "#{BuildDir}/lib/std:#{BuildDir}/lib/ieee"
VestsDir = "#{TestDir}/vests"
Prefix = "#{VestsDir}/vhdl-93"

unless Dir.exists? VestsDir
  puts "VESTs source missing from #{VestsDir}"
  puts
  puts "You can download this using"
  puts "  git clone https://github.com/nickg/vests.git"
  exit 1
end

$fail = 0
$pass = 0

def find_tests(path, compliant)
  tests = []
  Dir.foreach(path) do |item|
    next if item == 'README'
    next if item[0] == '.'
    next if item == 'CVS'
    next if item == 'simulator_failure'

    full = "#{path}/#{item}"
    if File.directory? full then
      find_tests full, (compliant and (item != 'non_compliant'))
    elsif item =~ /\.vhdl?$/
      tests << item
    end
  end

  unless tests.empty?
    Dir.chdir path
    puts "---- #{path.sub(Prefix.to_s, '')} ----".yellow
    tests.each do |t|
      cmd = "nvc -a #{path}/#{t}"
      cmd += " 2>/dev/null" unless compliant
      puts cmd
      r = system cmd
      if compliant then
        $fail += 1 unless r
        $pass += 1 if r
      else
        if r then
          puts "expected fail".red
          $fail += 1
        else
          $pass += 1
        end
      end
    end
    puts
  end
end

find_tests Prefix, true

puts "#{$pass} passes"
puts "#{$fail} failures"

File.open("#{VestsDir}/HISTORY", 'a') do |f|
  rev = IO::popen("cd #{TestDir} && git rev-parse --short HEAD").read.chomp
  f.printf("%20s %10s   %4d passes   %4d failures\n",
    Time.new.ctime, rev, $pass, $fail)
end
