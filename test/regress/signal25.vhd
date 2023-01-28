entity signal25 is
end entity;

architecture test of signal25 is
    type rec is record
        f : bit_vector;
    end record;

    signal v : bit_vector(1 to 3);
    signal r : rec(f(1 to 2));
begin

    p0: v(1 to r.f'right) <= (others => '1');
    p1: v(r.f'right + 1 to 3) <= (others => '0');

    check: process is
    begin
        wait for 1 ns;
        assert v = "110";
        wait;
    end process;

end architecture;
