#!/usr/bin/env ruby

require 'rubygems'
require 'pathname'
require 'colorize'
require 'timeout'
require 'open3'
require 'tmpdir'

if ARGV.length != 1 then
  puts "Usage: #{__FILE__} /path/to/ivtest"
  exit 1
end

TestDir = Pathname.new(__FILE__).realpath.dirname
BuildDir = Pathname.new(ENV['BUILD_DIR'] || Dir.pwd).realpath
LibPath = "#{BuildDir}/lib/std:#{BuildDir}/lib/ieee"
IvtestDir = Pathname.new(ARGV[0]).realpath
GitRev = IO::popen("git rev-parse --short HEAD").read.chomp
Tool = ENV['NVC'] || 'nvc'
ExpectFails = 1432

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

fails  = 0
passes = 0

["regress-vlg"].each do |list|
  File.open("#{IvtestDir}/#{list}.list").each_line do |line|
    cleaned = line.gsub(/#.*/, '').strip
    next if cleaned.empty?

    name, type, dir, flags = cleaned.split
        
    Dir.mktmpdir do |workdir|
      f = File.realpath "#{IvtestDir}/#{dir}/#{name}.v"
      cmd = "#{Tool} --work=work:#{workdir}/work -a #{f}"

      result = run_cmd cmd, (type == "CE")
      
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

File.open("#{IvtestDir}/HISTORY", 'a') do |f|
  f.printf("%20s %10s   %4d passes   %4d failures\n",
    Time.new.ctime, GitRev, passes, fails)
end

if fails > ExpectFails then
  puts "Expected #{ExpectFails} failures but have #{fails}"
  exit 1
end
