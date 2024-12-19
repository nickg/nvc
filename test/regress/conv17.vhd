entity conv17 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of conv17 is
    function to_logic (x : real) return std_logic is
    begin
        if x <= -1.0 then
            return '0';
        elsif x >= 1.0 then
            return '1';
        else
            return 'Z';
        end if;
    end function;

    signal s : std_logic := 'Z';
begin

    b1: block is
        port (p : out real := 0.0);
        port map (to_logic(p) => s);
    begin
        p <= 5.0 after 1 ns, 0.0 after 4 ns;
    end block;

    b2: block is
        port (p : out real := 0.0);
        port map (to_logic(p) => s);
    begin
        p <= 7.0 after 2 ns, -4.0 after 3 ns, -12.5 after 4 ns;
    end block;

    check: process is
    begin
        assert s = 'Z';
        wait for 1 ns;
        assert s = '1';
        wait for 1 ns;
        assert s = '1';
        wait for 1 ns;
        assert s = 'X';
        wait for 1 ns;
        assert s = '0';
        wait;
    end process;

end architecture;
