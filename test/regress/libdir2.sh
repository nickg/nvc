set -xe

pwd
which nvc

cat >mypack.vhd <<EOF
package mypack is
    constant k : integer := 1;
end package;
EOF

cat >ent.vhd <<EOF
entity libdir2 is
end entity;

library mylib;
use mylib.mypack.all;

architecture test of libdir2 is
begin

    main: process is
    begin
        assert k = 1;
        wait;
    end process;

end architecture;
EOF

nvc --work=MYLIB -a mypack.vhd
nvc -L. --work=WORK -a ent.vhd
nvc -L. --work=WORK -e libdir2
