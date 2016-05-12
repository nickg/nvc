entity jcore4 is
end entity;

architecture test of jcore4 is
    type rt is record
        x : bit_vector(1 to 3);
    end record;

    type at is array (integer range <>) of rt;

    signal a : at(1 to 3);
    signal c : integer := 0;
begin

    p1: process (a) is
    begin
        report "wakeup";
        c <= c + 1;
    end process;

    p2: process is
    begin
        wait for 1 ns;
        a(3).x(3) <= '1';
        wait for 1 ns;
        assert c = 2;
        wait;
    end process;

end architecture;
