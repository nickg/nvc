entity index1 is
end entity;

architecture test of index1 is
    type mem1_t is array (natural range <>) of bit_vector(7 downto 0);
    signal s1 : mem1_t(1 to 100);

    type mem2_t is array (natural range <>) of bit_vector(9 downto 0);
    signal s2 : mem2_t(1 to 50);
begin

    p1: process is
    begin
        for i in 1 to 20 loop
            s1(i) <= X"f0";
            s2(i * 2) <= (others => '1');
        end loop;
        wait;
    end process;

end architecture;
