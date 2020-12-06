package in_foo is
    type thing is (x, y);
end package;

-------------------------------------------------------------------------------

entity issue226 is
end entity;

architecture test of issue226 is
begin

    process is
        variable l : std.textio.line;   -- OK
    begin
        std.textio.write(l, string'("hello"));  -- OK
    end process;

    process is
        variable x : foo.in_foo.thing;  -- Error
    begin
    end process;

end architecture;
