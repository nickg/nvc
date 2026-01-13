set -xe

! nvc -a -Werror - 2>err <<EOF
entity test is
end entity;
architecture a of test is
begin
process is
begin
end process;
end architecture;
EOF

[ $? = 0 ]

grep "potential infinite loop" err
grep "this warning is treated as an error" err
