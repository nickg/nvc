entity bounds30 is
end entity;

architecture test of bounds30 is
    signal s : integer := -1;
begin

    main: process is
    begin
        wait for 1 ns;
        assert 2 ** s = 0;              -- Error, negative exponent
        wait;
    end process;

end architecture;
