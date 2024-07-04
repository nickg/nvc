entity implicit7 is
end entity;

architecture test of implicit7 is
    signal x : bit_vector(1 to 3);

    type t_nat_vec is array (natural range <>) of natural;
    signal count : t_nat_vec(x'range);
begin

    g: for i in x'range generate
        p: process is
        begin
            wait on x(i)'transaction;
            count(i) <= count(i) + 1;
        end process;
    end generate;

    check: postponed process is
    begin
        wait for 1 ns;
        assert count = (0, 0, 0);
        x <= "100" after 1 ns;
        wait for 1 ns;
        wait for 1 ns;
        assert count = (1, 1, 1);
        x <= "100" after 1 ns;
        wait for 2 ns;
        assert count = (2, 2, 2);
        x(3) <= '1' after 1 ns;
        wait for 2 ns;
        assert count = (2, 2, 3);

        wait;
    end process;

end architecture;
