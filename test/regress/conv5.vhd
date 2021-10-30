package pack is
    type int_vector is array (natural range <>) of natural;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    generic ( size : natural );
    port ( o1 : out int_vector(1 to size);
           o2 : out int_vector(1 to 3);
           i1 : in integer );
end entity;

architecture test of sub is
begin

    p1: process is
        variable tmp : int_vector(1 to size);
    begin
        assert i1 = 0;
        for i in 1 to size loop
            tmp(i) := i;
        end loop;
        o1 <= tmp;
        o2 <= (5, 6, 7);
        wait for 1 ns;
        assert i1 = 150;
        o1(1) <= 10;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity conv5 is
end entity;

use work.pack.all;

architecture test of conv5 is
    signal x1 : integer;
    signal x2 : integer;
    signal y  : int_vector(1 to 5);

    function sum_ints(v : in int_vector) return integer is
        variable result : integer := 0;
    begin
        for i in v'range loop
            result := result + v(i);
        end loop;
        return result;
    end function;

begin

    uut1: entity work.sub
        generic map ( size => 5)
        port map ( sum_ints(o1) => x1,
                   sum_ints(o2) => x2,
                   i1 => sum_ints(y) );

    p2: process is
    begin
        assert x1 = 0;
        assert x2 = 0;
        y <= (10, 20, 30, 40, 50);
        wait for 1 ns;
        assert x1 = 15;
        assert x2 = 18;
        wait for 1 ns;
        assert x1 = 24;
        wait;
    end process;

end architecture;
