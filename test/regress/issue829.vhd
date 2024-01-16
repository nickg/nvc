library ieee;
use ieee.std_logic_1164.all;

entity issue829 is
end entity;

architecture sim of issue829 is
    signal a : std_logic bus;
    disconnect a : std_logic after 5 ns;
    signal b : boolean;
    signal c : std_logic := '1';
begin

    c <= not c after 5 ns when now < 30 ns;

    BL : block (b) is
    begin
        a <= guarded c;
    end block;

    process
    begin
        assert a = 'U';
        wait for 0 ns;
        assert a = 'U';
        wait for 5 ns;
        assert a = 'Z';
        wait for 10 ns;
        assert a = 'Z';
        b <= true;
        wait for 0 ns;
        assert a = 'Z';
        wait for 5 ns;
        assert a = '0';
        wait for 5 ns;
        assert a = '1';
        wait for 5 ns;
        assert a = '0';
        wait for 10 ns;
        b <= false;
        assert a = '1';
        wait for 0 ns;
        assert a = '1';
        wait for 5 ns;
        assert a = 'Z';
        wait;
    end process;

end architecture;
