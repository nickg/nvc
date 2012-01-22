package p is
    function add_one_s(signal s : in integer) return integer;
    function event(signal s : in integer) return boolean;
end package;

package body p is

    function add_one_s(signal s : in integer) return integer is
    begin
        return s + 1;
    end function;

    function event(signal s : in integer) return boolean is
    begin
        return s'event;
    end function;

end package body;

entity func5 is
end entity;

use work.p.all;

architecture test of func5 is
    signal x : integer := 5;
begin

    process is
    begin
        assert add_one_s(x) = 6;
        x <= 2;
        wait for 0 ns;
        assert event(x);
        wait for 1 ns;
        assert add_one_s(x) = 3;
        assert not event(x);
        wait;
    end process;

end architecture;
