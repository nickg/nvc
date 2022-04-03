entity alias13 is
end entity;

architecture test of alias13 is
    type mat2d is array (natural range <>, natural range <>) of integer;
    signal s1 : mat2d(1 to 2, 1 to 2);
    alias a : mat2d is s1;              -- OK (2008)
begin

    main: process is
    begin
        s1 <= ((1, 2), (3, 4));
        wait for 1 ns;
        assert a = ((1, 2), (3, 4));
        assert a(1, 1) = 1;
        a(2, 2) <= 66;
        wait for 1 ns;
        assert s1 = ((1, 2), (3, 66));
        wait;
    end process;

end architecture;
