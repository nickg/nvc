package A_NG is
    procedure PROC(SEL: in integer);
end package;
package body A_NG is
    procedure PROC(SEL: in integer) is
    begin
        case SEL is
            when 0 | 1 =>         -- Used to crash in sem_hoist_for_loop_var
                for i in 0 to 3 loop
                end loop;
            when others =>
                null;
        end case;
    end procedure;
end package body;
