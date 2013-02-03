entity bounds5 is
end entity;

architecture test of bounds5 is

    type int_vec is array (natural range <>) of integer;

    signal s : int_vec(8 downto 0);

begin

    process is
    begin
        s(9 downto 1) <= (others => 1);
        wait;
    end process;

end architecture;
