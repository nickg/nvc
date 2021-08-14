entity arrayref4 is
end entity;

architecture test of arrayref4 is
    signal sticky : bit;
    signal shiftedFracY_d1 :  bit_vector(49 downto 0);
begin
    update: sticky <= '0' when (shiftedFracY_d1(23 downto 0)=(23 downto 0 => '0')) else '1';

    stim: process is
    begin
        wait for 1 ns;
        assert sticky = '1';
        shiftedFracY_d1(19 downto 0) <= X"00000";
        wait for 1 ns;
        assert sticky = '1';
        shiftedFracY_d1(24 downto 20) <= "00000";
        wait for 1 ns;
        assert sticky = '0';
        wait;
    end process;

end architecture;
