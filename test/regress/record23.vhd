entity record23 is
end entity;

architecture test of record23 is
    type rec1 is record
        x, y : integer;
    end record;

    type rec2 is record
        r : rec1;
    end record;

    signal s1 : rec1;
    signal s2 : rec2;
begin

    main: process is
    begin
        s1 <= (3, 4);
        wait for 1 ns;
        s2 <= (r => s1);                -- Error here
        wait for 1 ns;
        assert s2.r.x = 3;
        assert s2.r.y = 4;
        wait;
    end process;

end architecture;
