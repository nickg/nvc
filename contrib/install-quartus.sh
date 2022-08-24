#!/usr/bin/env bash
#
# Called by "nvc --install quartus".
#

. $(dirname $BASH_SOURCE)/functions.sh

if [ -n "$MSYSTEM" ]; then
  # MSYS2/Windows hack
  # Only do this if QUARTUS_ROOTDIR exists
  # Don't do this on Linux
  QUARTUS_ROOTDIR=$(cygpath -u $QUARTUS_ROOTDIR)
  echo QUARTUS_ROOTDIR=$QUARTUS_ROOTDIR
fi

if [ -z "$QUARTUS_ROOTDIR" ]; then
  cat >&2 <<EOF

Set the QUARTUS_ROOTDIR environment variable to the Quartus installation
directory.  For example:

  export QUARTUS_ROOTDIR="/opt/quartus18/quartus"

EOF
  exit 1
elif [ ! -d "$QUARTUS_ROOTDIR/eda/sim_lib" ]; then
  cat >&2 <<EOF
$QUARTUS_ROOTDIR/eda/sim_lib not found

Ensure QUARTUS_ROOTDIR points at a valid Quartus installation directory.

EOF
  exit 1
fi

echo "Using Quartus installation in $QUARTUS_ROOTDIR"
echo

src=$QUARTUS_ROOTDIR/eda/sim_lib

GLOBAL_OPTS="-M 32m"
A_OPTS="--relaxed"

for STD in 1993 2008; do

  analyse_list altera$(std_suffix $STD) <<EOF
$src/altera_primitives_components.vhd
$src/altera_primitives.vhd
EOF

  analyse_list altera_mf$(std_suffix $STD) <<EOF
$src/altera_mf_components.vhd
$src/altera_mf.vhd
EOF

  analyse_list lpm$(std_suffix $STD) <<EOF
$src/220pack.vhd
$src/220model.vhd
EOF

  analyse_list lpm$(std_suffix $STD) <<EOF
$src/220pack.vhd
$src/220model.vhd
EOF

  analyse_list altera_lnsim$(std_suffix $STD) <<EOF
$src/altera_lnsim_components.vhd
EOF

  analyse_list sgate$(std_suffix $STD) <<EOF
$src/sgate_pack.vhd
$src/sgate.vhd
EOF

  analyse_list arriaii$(std_suffix $STD) <<EOF
$src/arriaii_atoms.vhd
$src/arriaii_components.vhd
$src/arriaii_hssi_atoms.vhd
$src/arriaii_hssi_components.vhd
$src/arriaii_pcie_hip_atoms.vhd
$src/arriaii_pcie_hip_components.vhd
EOF

  analyse_list arriav$(std_suffix $STD) <<EOF
$src/arriav_atoms.vhd
$src/arriav_components.vhd
$src/arriav_hssi_components.vhd
$src/arriav_hssi_atoms.vhd
EOF

  analyse_list arriavgz$(std_suffix $STD) <<EOF
$src/arriavgz_atoms.vhd
$src/arriavgz_components.vhd
$src/arriavgz_hssi_components.vhd
$src/arriavgz_hssi_atoms.vhd
$src/arriavgz_pcie_hip_atoms.vhd
$src/arriavgz_pcie_hip_components.vhd
EOF

  analyse_list cycloneiii$(std_suffix $STD) <<EOF
$src/cycloneiii_atoms.vhd
$src/cycloneiii_components.vhd
EOF

  analyse_list cycloneiv$(std_suffix $STD) <<EOF
$src/cycloneiv_atoms.vhd
$src/cycloneiv_components.vhd
$src/cycloneiv_hssi_atoms.vhd
$src/cycloneiv_hssi_components.vhd
$src/cycloneiv_pcie_hip_atoms.vhd
$src/cycloneiv_pcie_hip_components.vhd
$src/cycloneive_atoms.vhd
$src/cycloneive_components.vhd
EOF

  analyse_list cyclonev$(std_suffix $STD) <<EOF
$src/cyclonev_atoms.vhd
$src/cyclonev_components.vhd
$src/cyclonev_hssi_atoms.vhd
$src/cyclonev_hssi_components.vhd
EOF

  analyse_list cyclone10lp$(std_suffix $STD) <<EOF
cyclone10lp_atoms.vhd
cyclone10lp_components.vhd
EOF

  analyse_list max$(std_suffix $STD) <<EOF
$src/max_atoms.vhd
$src/max_components.vhd
EOF

  analyse_list maxii$(std_suffix $STD) <<EOF
$src/maxii_atoms.vhd
$src/maxii_components.vhd
EOF

  analyse_list maxv$(std_suffix $STD) <<EOF
$src/maxv_atoms.vhd
$src/maxv_components.vhd
EOF

  analyse_list fiftyfivenm$(std_suffix $STD) <<EOF
fiftyfivenm_atoms.vhd
fiftyfivenm_components.vhd
EOF

done
