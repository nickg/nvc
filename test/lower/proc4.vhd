package pack is
    procedure does_not_wait;

    attribute never_waits : boolean;
    attribute never_waits of does_not_wait : procedure is true;
end package;

package body pack is
    procedure does_not_wait is
    begin
    end procedure;
end package body;

-------------------------------------------------------------------------------

entity proc4 is
end entity;

use work.pack.all;

architecture test of proc4 is
begin

    p1: process is
    begin
        does_not_wait;                  -- Should use fcall
        wait;
    end process;

end architecture;
