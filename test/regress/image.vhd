package p is
    type enum1 is (A, B);

    procedure print_char(c : in character);
    procedure print_enum1(e : in enum1);
end package;

package body p is
    procedure print_char(c : in character) is
    begin
        report character'image(c);
    end procedure;

    procedure print_enum1(e : in enum1) is
    begin
        report enum1'image(e);
    end procedure;
end package body;

entity image is
end entity;

use work.p.all;

architecture test of image is
    type enum2 is (C, D);
begin

    process is
        variable i : integer;
        variable e1 : enum1;
        variable e2 : enum2;
    begin
        report integer'image(4);
        report integer'image(-42);
        i := 73;
        report integer'image(i);
        report "i=" & integer'image(i) & " units";
        report character'image('c');
        print_char('X');
        wait for 10 ps;
        report time'image(now);
        e1 := A;
        e2 := C;
        wait for 1 ns;
        report enum1'image(e1);
        report enum2'image(e2);
        print_enum1(e1);
        wait;
    end process;

end architecture;
