#!/usr/bin/env ruby

require 'rubygems'
require 'pathname'
require 'colorize'
require 'timeout'

TestDir = Pathname.new(__FILE__).realpath.dirname
BuildDir = Pathname.new(ENV['BUILD_DIR'] || Dir.pwd).realpath
LibPath = "#{BuildDir}/lib/std"

def read_tests
  tests = []
  File.open(TestDir + "regress/testlist.txt").each_line do |l|
    parts = l.split /\s+/
    flags = parts[1].split /,/
    tests << { :name => parts[0], :flags => flags }
  end
  tests
end

def nvc
  "#{BuildDir}/src/nvc"
end

def run_cmd(c, invert=false)
  File.open('out', 'a') do |f|
    f.puts c
  end
  
  pid = fork
  exec("#{c} >>out 2>&1") if pid.nil?
  begin
    timeout(1) do
      Process.wait
      fail unless $?.exitstatus == (invert ? 1 : 0)
    end
  rescue Timeout::Error
    Process.kill 'TERM', pid
    raise
  end
end

def analyse(t)
  run_cmd "#{nvc} -a #{TestDir}/regress/#{t[:name]}.vhd"
end

def elaborate(t)
  run_cmd "#{nvc} -e #{t[:name]}"
end

def run(t)
  run_cmd "#{nvc} -r #{t[:name]}", t[:flags].member?('fail')
end

def check(t)
  if t[:flags].member? 'gold' then
    fname = TestDir + "regress/gold/#{t[:name]}.txt"
    out_lines = File.open('out').lines
    File.open(fname).each_line do |match_line|
      unless out_lines.any? do |output_line|
          output_line.include? match_line.chomp
        end
      then
        puts "failed (no match)".red
        print match_line.chomp
        return
      end
    end
  end
  print "ok".green
end

def mkdir_p(n)
  begin
    Dir.mkdir n
  rescue Errno::EEXIST
  end
end

mkdir_p 'logs'
Dir.chdir 'logs'

ENV['NVC_LIBPATH'] = LibPath

passed = 0
failed = 0

read_tests.each do |t|
  printf "%15s : ", t[:name]
  mkdir_p t[:name]
  Dir.chdir t[:name] do
    File.unlink 'out' if File.exists? 'out'
    begin
      analyse t
      elaborate t
      run t
      check t
      passed += 1
    rescue RuntimeError
      puts "failed".red
      File.open('out').each_line do |l|
        puts l
      end
      failed += 1
    rescue Timeout::Error
      puts "failed (timeout)".red
      failed += 1
    end
    puts
  end
end

exit failed
