entity issue1175 is
end entity;

architecture test of issue1175 is
    signal s : bit_vector(1 to 3);
    signal i : bit;
begin

    b: block is
        port ( i : in bit; p : out bit_vector );
        port map ( i, s );
    begin
        p <= (others => i);
    end block;

    process is
    begin
        i <= '1';
        wait for 1 ns;
        assert s = "111";
        i <= '0';
        wait for 1 ns;
        assert s = "000";
        wait;
    end process;

end architecture;
