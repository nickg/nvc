entity event1 is
end entity;

architecture test of event1 is
    signal si : integer := 0;
    event e1 : boolean := si > 0;
    event e2 : integer := si + 1;
begin

    si <= si + 1 after 1 ns when si < 10;

    -- We can use events like variables in expressions in which case they are
    -- evaluated directly
    process is
    begin
        assert not e1;
        assert e2 = 1;
        wait for 1 ns;
        assert e1;
        assert e2 = 2;
        wait;
    end process;

end architecture;
