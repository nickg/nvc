#!/usr/bin/env ruby

require 'rubygems'
require 'pathname'
require 'colorize'
require 'timeout'
require 'open3'
require 'tmpdir'

TestDir = Pathname.new(__FILE__).realpath.dirname
BuildDir = Pathname.new(ENV['BUILD_DIR'] || Dir.pwd).realpath
LibPath = "#{BuildDir}/lib/std:#{BuildDir}/lib/ieee"
VestsDir = ARGV[0] || "#{TestDir}/vests"
Prefix = "#{VestsDir}/vhdl-93"
GitRev = IO::popen("git rev-parse --short HEAD").read.chomp
Tool = 'nvc'
Billowitch = "#{Prefix}/billowitch/compliant"
ExpectFails = 87

ENV['NVC_COLORS'] = 'always'

def run_cmd(c)
  Timeout.timeout(10) do
    stdout, stderr, status = Open3.capture3(c)
    if status != 0 then
      puts
      puts c.magenta
      puts stdout
      puts stderr
    end
    return status == 0
  end
end

def guess_top(src)
  best = nil
  File.open(src).lines do |l|
    if m = l.match(/^(?:entity|configuration) *(\w+)/i) then
      best = m.captures[0]
    end
  end
  return best
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

Dir.foreach(Billowitch) do |item|
  next unless item =~ /\.vhdl?$/

  Dir.mktmpdir do |tmpdir|
    f = File.realpath "#{Billowitch}/#{item}"
    top = guess_top f
    #cmd = "#{Tool} --work=work:#{tmpdir} -a #{f} -e #{top} -r"
    cmd = "#{Tool} --force-init --work=work:#{tmpdir} -a #{f}"
    if run_cmd cmd then
      passes += 1
      print '+'.green
    else
      fails += 1
    end
  end
end

puts
puts "#{passes} passes"
puts "#{fails} failures"

File.open("#{VestsDir}/HISTORY", 'a') do |f|
  f.printf("%20s %10s   %4d passes   %4d failures\n",
    Time.new.ctime, GitRev, passes, fails)
end

if fails > ExpectFails then
  puts "Expected #{ExpectFails} failures but have #{fails}"
  exit 1
end
