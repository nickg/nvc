package assert1 is
    procedure do_report;
    procedure do_assert(x : integer);
end package;

package body assert1 is

    procedure do_report is
    begin
        report "hello world";
    end procedure;

    procedure do_assert(x : integer) is
    begin
        assert x >= 0 report integer'image(x) & " negative" severity warning;
        assert x < 100 report "too big" severity failure;
    end procedure;

end package body;
