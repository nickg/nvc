set -xe

nvc -a - <<EOF
entity cmdline22 is
  generic ( TimingChecksOn : boolean := true );
end entity;

architecture test of cmdline22 is
begin
  process is
  begin
    report "TimingChecksOn=" & boolean'image(TimingChecksOn);
    wait;
  end process;
end architecture;
EOF

nvc -e cmdline22 --no-save -r 2>&1 | tee stdout1
grep "TimingChecksOn=true" stdout1

nvc --notimingchecks -e cmdline22 --no-save -r 2>&1 | tee stdout2
grep "disabling VITAL timing check" stdout2
grep "TimingChecksOn=false" stdout2
