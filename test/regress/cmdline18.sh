set -xe

nvc -a - <<EOF
entity test is
end entity;
architecture a of test is
begin
process is
begin
  report "note" severity note;
  report "warning" severity warning;
  report "error" severity error;
  report "failure" severity failure;
  wait;
end process;
end architecture;
EOF

! nvc --stderr=none -e test --no-save -r 2>stderr 1>stdout

[ $? = 0 ]

grep failure stdout
[ -f stderr ]
[ ! -s stderr ]

