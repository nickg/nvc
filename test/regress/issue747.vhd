entity issue747 is
generic (
    L : natural := 1
);
end entity;

architecture test of issue747 is
    signal s : bit_vector(L-1 downto 0);
begin

    process
    begin
        wait for 1 ns;
        s(10) <= '0'; -- no check done for out of bound
        s(12 downto 11) <= "11"; -- check is done here
        wait for 1 ns;
        report "SIMULATION ENDED" severity failure;
        wait;
    end process;

end architecture;
