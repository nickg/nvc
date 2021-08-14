entity bounds1 is
end entity;

architecture test of bounds1 is
    type int_vec is array (natural range <>) of integer;
begin

    p1: process is
        variable v : int_vec(0 to 9) := (others => 0);
        variable k : integer range 0 to 9;
    begin
        assert v(k) = 1;                -- Should elide
        assert v(k + 1) = 1;            -- Cannot elide
        wait;
    end process;


end architecture;
