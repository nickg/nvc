entity conv12 is
end entity;

architecture test of conv12 is
    function half (x : integer) return integer is
    begin
        return x / 2;
    end function;

    function double (x : integer) return integer is
    begin
        return x * 2;
    end function;

    signal s : integer := 0;
begin

    b1: block is
        port ( p : in integer );
        port map ( p => half(s) );
    begin
        check: process is
        begin
            assert p = 0;
            wait for 1 ns;
            assert p = 0;
            wait for 2 ns;
            assert p = 5;
            wait;
        end process;
    end block;

    b2: block is
        port ( p : in integer );
        port map ( p => double(s) );
    begin
        check: process is
        begin
            assert p = 0;
            wait for 1 ns;
            assert p = 2;
            wait for 2 ns;
            assert p = 20;
            wait;
        end process;
    end block;

    s <= 1 after 1 ns, 10 after 2 ns;

end architecture;
