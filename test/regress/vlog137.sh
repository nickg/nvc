set -xe

# Regression: multi-top elaboration.  Two unrelated VHDL tops
# passed to a single `-e` elaborate as siblings of the synthetic
# anonymous root; each emits its own process output during
# simulation.

pwd
which nvc

cat >vlog137_a.vhd <<'EOF'
entity vlog137_a is end entity;
architecture arch of vlog137_a is
begin
    process begin
        report "top A here";
        wait;
    end process;
end architecture;
EOF

cat >vlog137_b.vhd <<'EOF'
entity vlog137_b is end entity;
architecture arch of vlog137_b is
begin
    process begin
        report "top B here";
        wait;
    end process;
end architecture;
EOF

nvc -a vlog137_a.vhd vlog137_b.vhd -e vlog137_a vlog137_b -r 2>out

grep "top A here" out
grep "top B here" out
