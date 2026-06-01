set -xe

nvc -a - <<EOF
entity cmdline20 is
end entity;

architecture wrong of cmdline20 is
begin
  assert false report "wrong architecture selected" severity failure;
end architecture;

architecture arch of cmdline20 is
begin
  process is
  begin
    report "PASSED" severity note;
    wait;
  end process;
end architecture;
EOF

nvc -e 'cmdline20(arch)' -r
