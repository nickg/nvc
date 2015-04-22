package A_NG is
end package;
package body A_NG is
    procedure PROC(S: in integer; Q: out integer) is
    begin
        -- Spurious error without forward declaration
        PROC(S,Q);
    end procedure;
end package body;
