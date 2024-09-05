entity ename15 is
end entity;

architecture test of ename15 is
    type t_three_ints is array (1 to 3) of integer;
    signal s : bit_vector(1 to 3);
begin

    check: process is
    begin
        assert <<signal b.s : t_three_ints>> = (0, 0, 0);
        assert <<signal b.p : bit_vector>> = "000";
        s <= "101";
        wait for 2 ns;
        assert <<signal b.s : t_three_ints>> = (1, 2, 3);
        assert <<signal b.p : bit_vector>> = "101";
        wait;
    end process;

    b: block is
        port ( p : in bit_vector);
        port map ( s );
        signal s : t_three_ints := (0, 0, 0);
    begin
        s <= (1, 2, 3) after 1 ns;
    end block;

end architecture;
