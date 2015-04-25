entity slice4 is
end entity;

architecture test of slice4 is

    procedure proc1(x : in string) is
    begin
        report x;
        report integer'image(x'left);
        report integer'image(x'right);
        report boolean'image(x'ascending);
        assert x'left = 1;
        assert x'right = 5;
        assert x'ascending;
        assert x = "hello";
    end procedure;

    procedure proc2(x : in string) is
    begin
        proc1(x(x'range));
    end procedure;

begin

    process is
        variable s : string(1 to 5) := "hello";
    begin
        proc2(s);
        wait;
    end process;

end architecture;
