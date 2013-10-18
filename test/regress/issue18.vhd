entity sub is
    port (
        x, y : in bit_vector(3 downto 0);
        z    : out bit_vector(3 downto 0) );
end entity;

architecture test of sub is
begin

    z <= x and y;

end architecture;

-------------------------------------------------------------------------------

entity issue18 is
end entity;

architecture test of issue18 is
    signal s   : bit_vector(7 downto 0);
    constant c : bit_vector(7 downto 0) := X"AA";
begin

    sub_i: entity work.sub
        port map (
            x => c(3 downto 0),
            y => s(3 downto 0),
            z => s(7 downto 4) );

    process is
    begin
        s(3 downto 0) <= X"A";
        wait for 1 ns;
        assert s = X"AA";
        s(3 downto 0) <= X"0";
        wait for 1 ns;
        assert s = X"00";
        s(3 downto 0) <= X"F";
        wait for 1 ns;
        assert s = X"AF";
        wait;
    end process;

end architecture;
