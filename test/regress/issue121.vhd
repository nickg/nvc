package A is
    procedure PROC_A(I:in integer; O:out integer; Z:out boolean);
    procedure PROC_B(I:in integer; O:out integer; Z:out boolean);
    procedure PROC_C(I:in integer; O:out integer; Z:out boolean);
end package;
package body A is
    procedure PROC_A(I:in integer; O:out integer; Z:out boolean) is
    begin
        -- Used to abort calling forward-declared procedure
        PROC_B(I,O,Z);
    end procedure;
    procedure PROC_B(I:in integer; O:out integer; Z:out boolean) is
    begin
        PROC_C(I,O,Z);
    end procedure;
    procedure PROC_C(I:in integer; O:out integer; Z:out boolean) is
    begin
        O := I;
        Z := (I = 0);
    end procedure;
end package body;

-------------------------------------------------------------------------------

entity issue121 is
end entity;

use work.A.all;

architecture test of issue121 is
begin

    process is
        variable o : integer;
        variable z : boolean;
    begin
        proc_a(1, o, z);
        assert o = 1;
        assert not z;
        proc_a(0, o, z);
        assert o = 0;
        assert z;
        wait;
    end process;

end architecture;
