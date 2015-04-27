package A_NG is
    type      A_NG_TYPE is record
                  debug         : integer;
    end record;
    procedure PROC_B(B_ARG:inout A_NG_TYPE; B_VAL:out integer);
end A_NG;
package body A_NG is
    procedure PROC_A(A_ARG:inout A_NG_TYPE) is
    begin
        A_ARG.debug := A_ARG.debug + 1;
    end procedure;
    procedure PROC_B(B_ARG:inout A_NG_TYPE; B_VAL:out integer) is
        procedure PROC_C(C_VAL:out integer) is
        begin
            PROC_A(B_ARG);
            C_VAL := B_ARG.debug;
        end procedure;
    begin
        PROC_C(B_VAL);
    end procedure;
end A_NG;

-------------------------------------------------------------------------------

entity issue146 is
end entity;

use work.a_ng.all;

architecture test of issue146 is
begin

    process is
        variable a_arg : a_ng_type := ( debug => 4 );
        variable c_val : integer;
    begin
        proc_b(a_arg, c_val);
        assert c_val = 5;
        wait;
    end process;

end architecture;
