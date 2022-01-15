entity concat is
end entity;

architecture test of concat is
    type int_vec is array (natural range <>) of integer;
begin

    p1: process is
        variable v : int_vec(1 to 3);
    begin
        v := (1, 2) & 3;
        wait;
    end process;

end architecture;
