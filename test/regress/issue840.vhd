entity issue840 is
end entity;

architecture test of issue840 is
    type t_array2d is array (natural range <>, natural range <>) of bit;
    subtype t_sub is t_array2d(1 to 3, 1 to 3);

    signal s : t_sub;
    signal t : integer;
begin

    process is
    begin
        s <= ("101", "110", "111");
        t <= 1;
        wait for 1 ns;
        s <= ("110", "000", "101");
        t <= 2;
        wait for 1 ns;
        wait;
    end process;

end architecture;
