#!/usr/bin/env ruby

require 'rubygems'
require 'pathname'
require 'colorize'

TestDir = Pathname.new(__FILE__).realpath.dirname
BuildDir = Dir.pwd
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

def run_cmd(c)
  File.open('out', 'a') do |f|
    f.puts c
  end
  fail unless system("#{c} >>out 2>&1")
end

def analyse(t)
  run_cmd "#{nvc} -a #{TestDir}/regress/#{t[:name]}.vhd"
end

def elaborate(t)
  run_cmd "#{nvc} -e #{t[:name]}"
end

def run(t)
  run_cmd "#{nvc} -r #{t[:name]}"
end

def check(t)
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
    end
    puts
  end
end

exit failed
