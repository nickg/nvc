entity bounds5 is
end entity;

architecture test of bounds5 is

    type int_vec is array (natural range <>) of integer;

    signal s : int_vec(8 downto 0);

begin

    process is
        variable k : integer;
    begin
        k := 9;
        wait for 1 ns;
        s(k downto 1) <= (others => 1);
        wait;
    end process;

end architecture;
