package pack is
    type rec is record
        x : integer;
        y : bit_vector;
    end record;

    type rec_array is array (natural range <>) of rec;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port (
        i  : in rec_array;
        o1 : out integer_vector;
        o2 : out bit_vector );
end entity;

architecture test of sub is
begin

    g: for n in i'range generate
        constant stride : natural := i(n).y'length;
    begin
        o1(n) <= i(n).x;
        o2(1 + (n-1)*stride to n*stride) <= i(n).y;
    end generate;

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity elab33 is
end entity;

architecture test of elab33 is
    signal a : rec_array(1 to 2)(y(1 to 3));
    signal b : integer_vector(1 to 2);
    signal c : bit_vector(1 to 6);
begin

    u : entity work.sub
        port map ( a, b, c );

    check: process is
    begin
        a(1) <= (y => "101", x => 2);
        wait for 1 ns;
        assert b = (2, integer'left);
        assert c = "101000";
        a(2).x <= 5;
        a(2).y <= "110";
        wait for 1 ns;
        assert b = (2, 5);
        assert c = "101110";

        wait;
    end process;

end architecture;
