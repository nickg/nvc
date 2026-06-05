#!/usr/bin/env ruby

require 'rubygems'
require 'pathname'
require 'colorize'
require 'timeout'
require 'open3'
require 'tmpdir'

if ARGV.length < 1 || ARGV.length > 2 then
  puts "Usage: #{__FILE__} /path/to/ivtest [test-list]"
  exit 1
end

TestDir = Pathname.new(__FILE__).realpath.dirname
BuildDir = Pathname.new(ENV['BUILD_DIR'] || Dir.pwd).realpath
LibPath = "#{BuildDir}/lib/std:#{BuildDir}/lib/ieee"
IvtestDir = Pathname.new(ARGV[0]).realpath
TestList = Pathname.new(ARGV[1] || "#{TestDir}/ivtest.list").realpath
GitRev = IO::popen("git rev-parse --short HEAD").read.chomp
Tool = ENV['NVC'] ? Pathname.new(ENV['NVC']).realpath.to_s : 'nvc'
ExpectFails = 1240

ENV['NVC_COLORS'] = 'always'

def run_cmd(c, expfail, gold, check_passed)
  Open3.popen3(c) do |i, o, e, t|
    i.close

    output = nil
    err = nil
    begin
      Timeout::timeout(5) do
        output = o.read.force_encoding("ISO-8859-1")
        err = e.read.force_encoding("ISO-8859-1")
      end
    rescue Timeout::Error
      Process.kill("KILL", t.pid)
      output = 'Timeout!'
    end

    status = t.value

    if err =~ /Caught signal/ or (c =~ /-r/ and output =~ /FAILED/i) then
      puts
      puts c.magenta
      puts output
      puts err
      return false
    elsif status != 0 and not expfail then
      puts
      puts c.magenta
      puts output
      puts err
      return false
    elsif status == 0 and expfail then
      puts
      puts c.magenta
      puts output
      puts err
      puts "expected failure!".magenta
      return false
    elsif gold then
      expected = File.read(gold)
      if output != expected
        puts
        puts c.magenta
        puts "Output did not match gold file: #{gold}".red
        puts ">>>> Expected".yellow
        puts expected
        puts "<<<< Actual".yellow
        puts output
        return false
      end
    elsif check_passed and status == 0 and !(output =~ /PASSED/i) then
      puts
      puts c.magenta
      puts output
      puts err
      puts "did not print PASSED!".magenta
      return false
    end

    return true
  end
end

def get_module_name(f, flags)
  if flags[:module] then
    return flags[:module]
  end

  if File.exist?(f) then
    File.readlines(f).reverse.each do |line|
      case line
      when /^(?:module|program)\s+(\w+)/
        return $1
      when /^`include\s+"(.*)"/
        return get_module_name($1, flags) || next
      end
    end
  end

  puts "Cannot find module name for #{f}".red
  return nil
end

def parse_flags(args)
  flags = {}

  args.each do |arg|
    case arg
    when /^gold=(.+)$/
      flags[:gold] = Regexp.last_match(1)
    when /^unordered=(.+)$/
      flags[:unordered] = Regexp.last_match(1)
    when /^diff=([^:]+):([^:]+)(?::(\d+))?$/
      flags[:diff] = {
        file1: Regexp.last_match(1),
        file2: Regexp.last_match(2),
        skip_lines: Regexp.last_match(3)&.to_i || 0
      }
    else
      flags[:module] = arg
    end
  end

  flags
end

fails  = 0
passes = 0

Dir.chdir IvtestDir

buffer = ""
File.open(TestList).each_line do |line|
  cleaned = line.gsub(/#.*/, '').strip
  next if cleaned.empty?

  if cleaned.end_with?("\\")
    buffer << cleaned.chomp("\\") # strip the backslash and append
    next
  else
    buffer << cleaned
  end

  name, typelist, dir, *rest = buffer.split

  type, *options = typelist.split(",")

  aflags = options.map do |o|
    case o
    when "-g2001"
      "--keywords=1364-2001"
    when "-g2001-noconfig"
      "--keywords=1364-2001-noconfig"
    when "-g2005-sv"
      "--keywords=1800-2005"
    when "-g2005-sv"
      "--keywords=1800-2005"
    when "-g2009"
      "--keywords=1800-2009"
    when "-g2012"
      "--keywords=1800-2012"
    else
      puts "Unknown iverilog option #{o}".yellow
    end
  end.join(" ")

  flags = parse_flags(rest)

  Dir.mktmpdir do |workdir|
    f = File.realpath "#{IvtestDir}/#{dir}/#{name}.v"
    m = get_module_name(f, flags)

    if type == "CE" || type == "CO" then
      cmd = "#{Tool} --work=work:#{workdir}/work -a #{aflags} --no-save #{f}"
    else
      cmd = "#{Tool} --seed=0 --work=work:#{workdir}/work -a #{aflags} " +
            "--no-save #{f} -e --jit --no-save #{m} -r"
    end

    gold = flags[:gold] ? "#{IvtestDir}/gold/#{flags[:gold]}" : nil

    result = run_cmd cmd, (type == "CE" || type == "RE"), gold, type != "CO"

    if result then
      passes += 1
      print '+'.green
    else
      fails += 1
    end
  end

  buffer = ""
end

puts
puts "#{passes} passes"
puts "#{fails} failures"

unless ARGV[1]
  File.open("#{IvtestDir}/HISTORY", 'a') do |f|
    f.printf("%20s %10s   %4d passes   %4d failures\n",
             Time.new.ctime, GitRev, passes, fails)
  end
end

if fails > ExpectFails then
  puts "Expected #{ExpectFails} failures but have #{fails}"
  exit 1
end
