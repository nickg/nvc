package p is
    constant cp : integer;
end package p;

package body p is
    constant c : integer := 1;
    alias ca : integer is c;
    constant cp : integer := ca;
end package body p;

use work.p.all;

entity alias8 is
begin
    assert (cp = 1) report "should not assert" severity failure;
end entity;

architecture test of alias8 is
begin
end architecture test;
