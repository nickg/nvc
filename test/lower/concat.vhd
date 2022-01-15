entity concat is
end entity;

architecture test of concat is
    type int_vec is array (natural range <>) of integer;
    signal s : int_vec(1 to 3);
begin

    p1: process is
        variable v : int_vec(1 to 3);
    begin
        v := (1, 2) & 3;
        wait;
    end process;

    p2: process is
    begin
        s <= (1, 2) & 3;
        s <= 4 & 5 & 6;
        wait;
    end process;

end architecture;
