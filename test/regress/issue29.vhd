entity sub is
    generic (
        width : integer );
    port (
        x : in bit_vector(width - 1 downto 0);
        y : in bit_vector;
        z : out bit_vector(width - 1 downto 0) );
end entity;

architecture test of sub is
begin

    z <= x and y after 1 us;

end architecture;

-------------------------------------------------------------------------------

entity issue29 is
end entity;

architecture rtl of issue29 is
    signal z : bit_vector(7 downto 0);
begin

    sub_i: entity work.sub
        generic map (
            width => 8 )
        port map (
            x => X"ab",
            y => X"cd",
            z => z );

    process is
    begin
        wait for 2 us;
        assert z = "10001001";
        wait;
    end process;

end architecture;
