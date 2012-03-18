package p is
    procedure print_char(c : in character);
end package;

package body p is
    procedure print_char(c : in character) is
    begin
        report character'image(c);
    end procedure;
end package body;

entity image is
end entity;

use work.p.all;

architecture test of image is
begin

    process is
        variable i : integer;
    begin
        report integer'image(4);
        report integer'image(-42);
        i := 73;
        report integer'image(i);
        report "i=" & integer'image(i) & " units";
        report character'image('c');
        print_char('X');
        wait;
    end process;
    
end architecture;
