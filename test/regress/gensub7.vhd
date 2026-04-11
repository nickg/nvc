library ieee;
use ieee.numeric_std.all;
use ieee.numeric_bit.all;

entity gensub7 is
end entity;

architecture test of gensub7 is

    function generic_sum
        generic (
            type t;
            init : t;
            function "+" (l : t; r : integer) return t is <>)
        return t
    is
        variable rv : init'subtype := init;
    begin
        for i in 1 to 3 loop
            rv := rv + 1;
        end loop;
        return rv;
    end function;

    function "+" (l : real; r : integer) return real is
    begin
        return l + real(r);
    end function;

    function sum is new generic_sum generic map (
        t => ieee.numeric_std.unsigned,
        init => ieee.numeric_std.to_unsigned(0, 4));
    function sum is new generic_sum generic map (
        t => ieee.numeric_bit.unsigned,
        init => ieee.numeric_bit.to_unsigned(0, 4));
    function sum is new generic_sum generic map (
        t => integer,
        init => 0);
    function sum is new generic_sum generic map (
        t => real,
        init => 0.0);

begin

    p1: process is
        variable us : ieee.numeric_std.unsigned(3 downto 0);
        variable ub : ieee.numeric_bit.unsigned(3 downto 0);
        variable iv : integer;
        variable rv : real;
    begin
        us := sum;
        ub := sum;
        iv := sum;
        rv := sum;

        assert us = ieee.numeric_std.to_unsigned(3, 4);
        assert ub = ieee.numeric_bit.to_unsigned(3, 4);
        assert iv = 3;
        assert rv = 3.0;

        wait;
    end process;

end architecture;
