set -xe

pwd
which nvc

cat >ent.vhd <<EOF
entity e is end entity;
EOF

nvc -a ent.vhd

cat >arch.vhd <<EOF
architecture a of e is begin end architecture;
EOF

nvc -a arch.vhd

touch ent.vhd  # Now stale

nvc -e e 2>msgs

if ! grep "is older than its source file ent.vhd" msgs; then
  echo "missing expected message"
  exit 1
fi
