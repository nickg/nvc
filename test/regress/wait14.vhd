entity wait14 is
end entity;

architecture test of wait14 is
    signal v : bit_vector(1 to 3);
    signal n : integer range v'range := 3;
begin

    stim: process is
    begin
        wait for 1 ns;
        v(2) <= '1';                    -- Should not wake up p1
        wait for 1 ns;
        v(3) <= '1';
        n <= 1;
        wait;
    end process;

    p1: assert v(1) = '0' and now = 0 ns;

    p2: assert v(n) = '0';

end architecture;
