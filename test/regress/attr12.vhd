package pack is

    constant iname : string := pack'instance_name;

    procedure proc;
end package;

package body pack is

    procedure proc is
    begin
        report pack'instance_name & " <--";
        report iname & " <--";
    end procedure;

end package body;

-------------------------------------------------------------------------------

entity attr12 is
end entity;

use work.pack.all;

architecture test of attr12 is
begin

    process is
    begin
        proc;
        wait;
    end process;

end architecture;
