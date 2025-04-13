package pack is
    function func (x : integer) return integer;
end package;

package body pack is
    function func (x : integer) return integer is
    begin
        assert x > 0 severity failure;
        return x + 1;
    end function;
end package body;

-------------------------------------------------------------------------------

entity assert13 is
    generic ( g : integer := -1 );
end entity;

use work.pack.all;

architecture test of assert13 is
    signal s : bit;
    signal t : integer;
begin

    process (s) is
    begin
        if s = '1' then
            t <= func(g);               -- This is globally static and triggers
                                        -- an assertion failure if evaluated
        else
            t <= 0;
        end if;
    end process;

end architecture;
