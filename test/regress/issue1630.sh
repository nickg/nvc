set -xe

cat >issue1630.vhd <<EOF
entity issue1630 is
end entity;

architecture test of issue1630 is
begin
    i_dut : entity work.issue1630_dut;

    process is
    begin
        report "Testbench statement";
        wait;
    end process;
end architecture;
EOF

cat >issue1630_dut.vhd <<EOF
entity issue1630_dut is
end entity;

architecture test of issue1630_dut is
begin
    process is
    begin
        report "DUT statement";
        wait;
    end process;
end architecture;
EOF

nvc -a \
    issue1630_dut.vhd \
    issue1630.vhd \
    -e --cover=statement issue1630 \
    -r

nvc --cover-export \
    --format=cobertura \
    --relative=. \
    -o export.xml \
    issue1630.ncdb

diff -u $TESTDIR/regress/gold/issue1630.xml export.xml
