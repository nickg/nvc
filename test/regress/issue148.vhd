package A_NG is
    type      A_NG_TYPE is record
                  debug         : integer;
    end record;
    procedure PROC_B(B_ARG:inout A_NG_TYPE; B_VAL:out integer);
end A_NG;
package body A_NG is
    procedure PROC_A(A_ARG:inout A_NG_TYPE) is
    begin
        null;
    end procedure;
    procedure PROC_B(B_ARG:inout A_NG_TYPE; B_VAL:out integer) is
        variable  b_var : integer;
        procedure PROC_C(C_ARG:in integer;C_VAL:out integer) is
        begin
            PROC_A(B_ARG);
            C_VAL := B_ARG.debug + C_ARG;
        end procedure;
    begin
        b_var := 1;
        PROC_C(b_var,B_VAL);
    end procedure;
end A_NG;

-------------------------------------------------------------------------------

entity issue148 is
end entity;

use work.a_ng.all;

architecture test of issue148 is
begin

    process is
        variable a_ng : a_ng_type;
        variable tmp  : integer;
    begin
        a_ng.debug := 2;
        proc_b(a_ng, tmp);
        assert tmp = 3;
        wait;
    end process;

end architecture;
