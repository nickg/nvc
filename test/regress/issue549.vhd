package pack is
    constant C : integer;

    type rec is record
        x : integer;
        y : bit_vector(1 to C);
    end record;

end package;

package body pack is
    constant C : integer := 4;
end package body;

-------------------------------------------------------------------------------

entity issue549 is
end entity;

use work.pack.all;

architecture test of issue549 is

    constant def : rec := (x => 0, y => "0000");

    procedure modify (variable arg : inout rec) is
    begin
        arg.y(1) := '1';
    end procedure;

    procedure test (arg : in rec) is
        variable copy : rec := def;
    begin
        copy.y := arg.y;
        modify(copy);
        assert def.y = "0000";
        assert copy.y = "1110";
    end procedure;

begin

    p1: test((x => 1, y => "0110"));

end architecture;
