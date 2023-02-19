library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity predef3 is
end entity;

architecture test of predef3 is
    type sub_t is record
        f : unsigned(1 to 2);
    end record;

    function "=" (x, y : sub_t) return boolean is
    begin
        return false;
    end function;

    type rec_t is record
        x : unsigned(7 downto 0);
        y : integer;
        z : sub_t;
    end record;

    type mem_t is array (natural range <>) of sub_t;
begin

    p1: process is
        variable r : rec_t;
        variable m : mem_t(1 to 2);
    begin
        r.x := "10X01U-H";
        r.y := 5;
        r.z.f := "0U";
        wait for 1 ns;
        assert r = ("10X01U-H", 5, (f => "0U"));
        assert r /= ("10X01U-H", 5, (f => "0-"));

        m(1) := (f => "1U");
        m(2) := (f => "10");
        wait for 1 ns;

        assert m = ((f => "1U"), (f => "10"));

        wait;
    end process;

end architecture;
