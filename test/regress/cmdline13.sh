set -xe

pwd
which nvc

cat >file1.v <<EOF
module mod1;
endmodule
\`define BAR \`MSG
EOF

cat >file2.v <<EOF
module mod2;
  initial begin
    \`ifdef FLAG
      \$display(\`BAR);
    \`else
      \$fatal;
    \`endif
  end
endmodule
EOF

cat >expect.v <<EOF
module mod1;
endmodule

module mod2;
  initial begin

      \$display("PASSED");



  end
endmodule
EOF

nvc --preprocess --single-unit -DMSG='"PASSED"' -DFLAG file1.v file2.v \
  | diff -bu /dev/stdin expect.v

# Workaround bug in LLVM 14
unset NVC_JIT_THRESHOLD

nvc -a --single-unit -DMSG='"PASSED"' -DFLAG file1.v file2.v -e mod2 -r
