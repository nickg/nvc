package A_NG is
    type STATE_TYPE is (STATE_IDLE, STATE_0, STATE_1);
end package;
package body A_NG is
    procedure PROC is
        type     STATE_TYPE is (STATE_IDLE, STATE_A, STATE_B);
        variable state       :  STATE_TYPE;
    begin
        state := STATE_A;               -- Referenced wrong STATE_TYPE
    end procedure;
end package body;
