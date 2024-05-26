package pack is
    procedure do_wait;
end package;

package body pack is
    procedure do_wait is
    begin
        wait for 1 ns;
    end procedure;
end package body;

-------------------------------------------------------------------------------

entity wait28 is
end entity;

use work.pack.all;

architecture test of wait28 is
    signal x : bit;
begin

    x <= '1' after 1 ns, '0' after 2 ns;

    sens: process (x) is
    begin
        do_wait;                        -- Error
    end process;

end architecture;
