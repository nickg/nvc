package toplevel3_pack is
    type rec is record
        f : bit_vector;
    end record;

    subtype rec4 is rec(f(1 to 4));
end package;

-------------------------------------------------------------------------------

use work.toplevel3_pack.all;

entity toplevel3 is
    port ( r1 : inout rec4;             -- OK
           r2 : inout rec );            -- Error
end entity;

architecture test of toplevel3 is
begin
end architecture;
