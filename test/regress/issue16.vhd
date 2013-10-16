entity sub is
    port (
        x : in bit_vector(7 downto 0);
        y : out bit_vector(7 downto 0) );
end entity;

architecture test of sub is
begin
    y <= x;
end architecture;

-------------------------------------------------------------------------------

entity issue16 is
end entity;

architecture test of issue16 is
    signal y : bit_vector(7 downto 0);
begin

    sub_i: entity work.sub
        port map ( X"ab", y );

    process is
    begin
        wait for 1 ns;
        assert y = X"ab";
        wait;
    end process;

end architecture;
