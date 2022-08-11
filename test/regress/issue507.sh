set -xe

pwd
which nvc

nvc -a - <<EOF
entity test is
  generic (val : natural);
end entity test;
architecture beh of test is
begin
  
  process
  begin
    report "Test " & integer'image(val);
    wait;
  end process;

end architecture beh;
EOF

# Test elaborating the same top-level multiple times in parallel

nvc -e --no-save test-beh -gval=1 -r &
nvc -e --no-save test-beh -gval=2 -r &
nvc -e --no-save test-beh -gval=3 -r &
nvc -e --no-save test-beh -gval=4 -r &

wait $!

wait   # For remaining processes

if ls -l work/*.so; then
  echo "left .so files!"
  exit 1
fi

