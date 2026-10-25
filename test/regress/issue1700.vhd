entity issue1700 is
end entity;

use std.env.all;

architecture test of issue1700 is
    function resolved(s : boolean_vector) return boolean is
        variable result : boolean := false;
        variable failed : boolean := false;
    begin
        for i in s'range loop
            if s(i) then
                failed := failed or result;
                result := true;
            end if;
        end loop;

        assert not failed report "multiple drivers" severity error;
        return result;
    end function;

    subtype resolved_boolean is resolved boolean;

    type rec_t is record
        named1 : resolved_boolean;
        named2 : resolved_boolean;
        anon1  : resolved boolean;
        anon2  : resolved boolean;
    end record;

    signal r : rec_t := (others => false);
begin
    process is
    begin
        assert GetVhdlAssertCount(error) = 0;
        wait;
    end process;
end architecture;
