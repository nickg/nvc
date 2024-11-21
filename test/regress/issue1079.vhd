library std;
use std.textio.all;

entity issue1079 is
end entity;

architecture tb of issue1079 is

    procedure log(msg : string) is
        variable o : line;
        file OUTPUT: TEXT open WRITE_MODE is "STD_OUTPUT";
    begin
        write(o,string'("::"));
        write(o,msg);
        writeline(output,o);
    end procedure;

begin

    main: process
    begin
        log ("hello world 1"); -- OK
        log ("hello world 2"); -- abort with fatal
        log ("hello world 3");
        log ("hello world 4");
        log ("hello world 5");
        wait;
    end process;

end architecture;
