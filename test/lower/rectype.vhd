package rectype is

    type r1 is record
        x : integer;
    end record;

end package;

entity e is
end entity;

use work.rectype.all;

architecture a of e is
    type r2 is record
        x : r1;
    end record;

    signal s : r2;
begin

    process is
        type r3 is record
            x : r2;
        end record;
        variable v : r3;
    begin
        v.x := s;
        wait;
    end process;

end architecture;
