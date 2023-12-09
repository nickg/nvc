set -xe

nvc -a - <<EOF
entity cmdline9 is
end entity;

architecture test of cmdline9 is
begin
  p: assert false;
end architecture;
EOF

nvc -e cmdline9

ls -l work

[ -f work/_WORK.CMDLINE9.elab.so ] || exit 99

nvc -r cmdline9 && exit 66

nvc -a - <<EOF
entity cmdline9 is
end entity;

architecture test of cmdline9 is
begin
  p: process is
  begin
     report "running new code";
     wait;
  end process;
end architecture;
EOF

nvc -e cmdline9 --jit
nvc -r cmdline9

[ ! -f work/_WORK.CMDLINE9.elab.so ] || exit 22
