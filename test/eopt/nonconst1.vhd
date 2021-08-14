entity nonconst1 is
end entity;

architecture test of nonconst1 is

    type int_vec is array (natural range <>) of integer;

    signal s, t : int_vec(8 downto 0);

begin

    process is
        variable k : integer;
    begin
        k := 9;
        wait for 1 ns;
        s(k downto 1) <= (others => 1);
        t(8 downto k) <= (others => 1);
        wait;
    end process;

end architecture;
