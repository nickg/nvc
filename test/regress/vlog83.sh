set -xe

pwd
which nvc

# %m format specifier: verify that $display("%m") from a deeply-nested
# instance prints the canonical dotted hierarchical name.
# IEEE 1364 section 17.1.1.5 / IEEE 1800 section 21.2.1.6: %m expands to the
# calling scope's hierarchical name in dotted form.
#
# If nvc currently emits an internal encoding (e.g. parent%child or #N),
# this test correctly pinpoints the gap.

cat >vlog83.v <<'EOF'
module leaf;
    initial $display("%m");
endmodule

module mid;
    leaf grandchild ();
endmodule

module vlog83;
    mid child ();
endmodule
EOF

nvc -a vlog83.v
nvc -e vlog83 --no-save -r >stdout 2>stderr

# The canonical IEEE output is the dotted hierarchical path from the
# top-level instance down to the scope containing $display.
grep 'vlog83\.child\.grandchild' stdout
