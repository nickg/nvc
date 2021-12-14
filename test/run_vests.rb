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
VestsDir = Pathname.new(ARGV[0] || "#{TestDir}/vests").realpath
Prefix = "#{VestsDir}/vhdl-93"
GitRev = IO::popen("git rev-parse --short HEAD").read.chomp
Tool = 'nvc'
Billowitch = "#{Prefix}/billowitch/compliant"
ExpectFails = 94

ENV['NVC_COLORS'] = 'always'

def run_cmd(c)
  Open3.popen2e(c) do |i, oe, t|
    i.close

    output = nil
    begin
      Timeout::timeout(5) do
        output = oe.read
      end
    rescue Timeout::Error
      Process.kill("KILL", t.pid)
      output = 'Timeout!'
    end

    status = t.value

    if status != 0 then
      puts
      puts c.magenta
      puts output
    end

    return status == 0
  end
end

def guess_top(src)
  best = nil
  File.open(src).each_line do |l|
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

Dir.mktmpdir do |tmpdir|
  Dir.chdir tmpdir

  File.open("#{Billowitch}/compliant.exp").each_line do |line|
    next unless m = line.match(/^run_compliant_test +(\w+.vhdl?)(.*)$/)

    tc = m.captures[0]
    io = m.captures[1]

    Dir.mktmpdir do |workdir|
      f = File.realpath "#{Billowitch}/#{tc}"
      top = guess_top f
      cmd = "#{Tool} --force-init --work=work:#{workdir} -a #{f} -e #{top} -r"

      if run_cmd cmd then
        passes += 1
        print '+'.green
      else
        fails += 1
      end
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
