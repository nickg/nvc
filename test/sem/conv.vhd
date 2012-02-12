entity conv is
end entity;

architecture test of conv is
begin

    process is
        type e is (F, G, H);
        type a is array (integer range <>) of integer;
        type b is array (integer range <>) of integer;
        type c is array (integer range <>) of e;
        
        variable x : integer;
        variable y : a(1 to 3);
        variable z : b(1 to 3);
        variable w : c(1 to 3);
    begin
        x := integer(2);                -- OK
        x := integer(y);                -- Error
        y := z;                         -- Error
        y := a(z);                      -- OK
        w := c(y);                      -- Error
        wait;
    end process;

end architecture;
