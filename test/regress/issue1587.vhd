library Std;
use Std.Standard.all;
use Std.TextIO.all;

entity issue1587 is
end entity;

architecture bar of issue1587 is
    procedure p(i : integer) is
        file TCF : text open read_mode is "prom.S";
        variable L : line;
        variable st: string(1 to i);
        variable good : boolean;
    begin
        readline(TCF, L);
        read(L, st, good);
        report st;
    end procedure p;
begin
    process is
        file f : text open write_mode is "prom.S";
        variable l : line;
    begin
        swrite(l, "hello");
        writeline(f, l);
        file_close(f);
        p(0); -- st will be declared 1 to 0, ie null array
        wait;
    end process;
end architecture bar;
