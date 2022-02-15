package pack is
    type rec is record
        x, y : natural;
    end record;

    type rec_array is array (natural range <>) of rec;

    function rec_array_to_int (r : rec_array) return natural;
    function int_to_rec_array (x : natural) return rec_array;
end package;

package body pack is
    function rec_array_to_int (r : rec_array) return natural is
        variable sum : natural;
    begin
        for i in r'range loop
            sum := sum + r(i).x + r(i).y;
        end loop;
        return sum;
    end function;

    function int_to_rec_array (x : natural) return rec_array is
        variable r : rec_array(1 to 3);
    begin
        for i in 1 to 3 loop
            r(i).x := x / i;
            r(i).y := x * i;
        end loop;
        return r;
    end function;
end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port (
        i1 : in rec_array(1 to 3);
        i2 : in natural );
end entity;

architecture test of sub is
begin

    p1: process is
    begin
        assert i1 = (1 to 3 => (0, 0));
        assert i2 = 0;
        wait for 0 ns;
        assert i1 = ((10, 10), (5, 20), (3, 30));
        assert i2 = 21;
        wait for 2 ns;
        assert i2 = 22;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity conv8 is
end entity;

architecture test of conv8 is
    signal s1 : natural;
    signal s2 : rec_array(1 to 3);
begin

    uut: entity work.sub
        port map (
            i1 => int_to_rec_array(s1),
            i2 => rec_array_to_int(s2) );

    main: process is
    begin
        s2 <= ((1, 2), (3, 4), (5, 6));
        s1 <= 10;
        wait for 1 ns;
        s2(2).y <= 5;
        wait;
    end process;

end architecture;
