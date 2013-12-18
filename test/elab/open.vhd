entity open_bot is
    port (
        i : in integer;
        o : out integer;
        v : out bit_vector(3 downto 0) := X"f" );
end entity;

architecture test of open_bot is
begin

    v(1) <= '0';

    process (i) is
    begin
        o <= i + 1;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity open_top is
end entity;

architecture test of open_top is
    signal x : integer;
begin

    uut: entity work.open_bot
        port map ( x, open );

end architecture;
