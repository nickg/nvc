package pack is
    type int_array1 is array (integer range <>) of integer;
    type int_array2 is array (integer range <>) of integer;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port (
        x : in int_array1(1 downto 0);
        y : in int_array2(1 downto 0) );
end entity;

architecture test of sub is
begin

    process is
    begin
        assert y = (1, 2);
        wait on x;
        assert x = (4, 5);
        wait on x;
        assert x = (6, 7);
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity issue60 is
end entity;

use work.pack.all;

architecture test of issue60 is
    signal a : int_array2(1 downto 0);
begin

    sub_i: entity work.sub
        port map (
            x => int_array1(a),
            y => int_array2(int_array1'(1, 2)) );

    process is
    begin
        a <= (4, 5);
        wait for 1 ns;
        a <= (6, 7);
        wait;
    end process;

end architecture;
