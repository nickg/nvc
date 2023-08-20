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

entity image1 is
end entity;

use work.p.all;

architecture test of image1 is
    type enum2 is (C, D);
    type my_int is range 1 to 4;
    type my_phys is range 1 to 100
        units
            UNIT_1;
            UNIT_2 = 10 UNIT_1;
        end units;
begin

    process is
        variable i : integer;
        variable j : my_int;
        variable e1 : enum1;
        variable e2 : enum2;
    begin
        report integer'image(4);
        report integer'image(-42);
        i := 73;
        j := 3;
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
        report my_int'image(j);
        report my_phys'image(2 UNIT_2);
        report integer'image(0);
        report integer'image(10);
        report integer'image(integer'left);
        report integer'image(integer'right);
        wait;
    end process;

end architecture;
