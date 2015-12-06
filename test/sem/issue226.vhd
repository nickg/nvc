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
        variable x : ieee.std_logic_1164.std_logic;  -- Error
    begin
    end process;

end architecture;
