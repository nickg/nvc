package genpkg is
  generic (val : natural := 5;
           function plus (l, r : integer) return integer);
  procedure add (l : inout integer);
end genpkg;

package body genpkg is
  procedure add (l : inout integer) is
  begin
    l := plus (l, val);
  end add;
end genpkg;

package genpkg2 is
  generic (v : natural;
           type t1;
           package subgenpkg is new work.genpkg generic map (<>));
end genpkg2;

package my_adder_pkg is new work.genpkg generic map (val => open, plus => "+");

-------------------------------------------------------------------------------

entity genpack14 is
end entity;

use work.my_adder_pkg.all;

architecture test of genpack14 is
begin

    p1: process is
        variable x : integer := 0;
    begin
        add(x);
        assert x = 5;
        add(x);
        assert x = 10;
        wait;
    end process;

end architecture;
