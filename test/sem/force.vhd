entity force1 is
end entity;

architecture test of force1 is
    signal s : integer;
    signal b : bit;
begin

    p1: process is
        variable v : integer;
    begin
        s <= force out 1;               -- OK
        v <= force out 1;               -- Error
        s <= force out 1.2;             -- Error
        (s, s) <= force integer_vector'(1, 2);   -- Error
        s <= force 1;                   -- OK (default to in)
        wait;
    end process;

    b1: block is
        generic (g : bit);
        generic map ('1');
        port (i : in bit);
        port map (b);
    begin
        p2: process is
        begin
            i <= force in '0';          -- OK
            i <= force out '1';         -- Error
            g <= force in '0';          -- Error
            i <= force in true;         -- Error
        end process;
    end block;

    p2: process is
        variable v : integer;
    begin
        s <= release;                   -- OK
        v <= release out;               -- Error
        (s, s) <= release;              -- Error
        wait;
    end process;


end architecture;
