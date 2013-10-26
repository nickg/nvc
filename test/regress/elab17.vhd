entity sub is
    generic ( NUM : integer );
    port ( s : in bit );
end entity;

architecture test of sub is
begin

    process is
    begin
        wait for (NUM * 10 ns) + 1 ns;
        assert s = '1';
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity elab17 is
end entity;

architecture test of elab17 is
    signal vec : bit_vector(2 downto 0);
begin

    gen: for i in 0 to 2 generate
        signal s : bit;
    begin

        sub_i: entity work.sub
            generic map ( i )
            port map ( vec(i) );

    end generate;

    process is
    begin
        vec <= "001";
        wait for 10 ns;
        vec <= "010";
        wait for 10 ns;
        vec <= "100";
        wait for 10 ns;
        wait;
    end process;

end architecture;
