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
Tool = ENV['NVC'] || 'nvc'
Billowitch = "#{Prefix}/billowitch"
ExpectFails = 101

ENV['NVC_COLORS'] = 'always'

def run_cmd(c, expfail)
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

    if output =~ /Caught signal/ or (c =~ /-r/ and output =~ /FAILED TEST/) then
      puts
      puts c.magenta
      puts output
      return false
    elsif status != 0 and output =~ /PASSED TEST/ then
      return true   # Testing assertion failure
    elsif status != 0 and not expfail then
      puts
      puts c.magenta
      puts output
      return false
    elsif status == 0 and expfail then
      puts
      puts c.magenta
      puts output
      puts "expected failure!".magenta
      return false
    end

    return true
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

  puts
  puts "Billowitch, compliant".blue.bold
  puts

  File.open("#{Billowitch}/compliant/compliant.exp").each_line do |line|
    next unless m = line.match(/^run_compliant_test +(\w+.vhdl?)(.*)(?:#.*)?$/)

    tc = m.captures[0]
    io = m.captures[1]

    Dir.mktmpdir do |workdir|
      f = File.realpath "#{Billowitch}/compliant/#{tc}"
      top = guess_top f
      cmd = "#{Tool} --std=1993 --work=work:#{workdir}/work -a #{f} -e --jit --no-save #{top} -r"

      result = run_cmd cmd, false
      
      if result then
        passes += 1
        print '+'.green
      else
        fails += 1
      end
    end
  end
end

#
# Billowitch, non-compliant, analyser failure
#

Dir.mktmpdir do |tmpdir|
  Dir.chdir tmpdir

  puts
  puts "Billowitch, non-compliant, analyzer failure".blue.bold
  puts

  File.open("#{Billowitch}/non_compliant/analyzer_failure/non_compliant.exp").each_line do |line|
    next unless m = line.match(/^run_non_compliant_test +(\w+.vhdl?)(.*)(?:#.*)?$/)

    tc = m.captures[0]
    io = m.captures[1]

    Dir.mktmpdir do |workdir|
      f = File.realpath "#{Billowitch}/non_compliant/analyzer_failure/#{tc}"
      cmd = "#{Tool} --std=1993 --work=work:#{workdir}/work -a #{f}"

      result = run_cmd cmd, true

      if result then
        passes += 1
        print '+'.green
      else
        fails += 1
      end
    end
  end
end

#
# Billowitch, non-compliant, simulator failure
#

Dir.mktmpdir do |tmpdir|
  Dir.chdir tmpdir

  puts
  puts "Billowitch, compliant, simulator failure".blue.bold
  puts

  File.open("#{Billowitch}/non_compliant/simulator_failure/non_compliant.exp").each_line do |line|
    next unless m = line.match(/^run_non_compliant_test +(\w+.vhdl?)(.*)(?:#.*)?$/)

    tc = m.captures[0]
    io = m.captures[1]

    Dir.mktmpdir do |workdir|
      f = File.realpath "#{Billowitch}/non_compliant/simulator_failure/#{tc}"
      top = guess_top f
      cmd = "#{Tool} --std=1993 --work=work:#{workdir}/work -a #{f} -e --jit --no-save #{top} -r"

      result = run_cmd cmd, true

      if result then
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
