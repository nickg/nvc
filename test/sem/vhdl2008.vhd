entity vhdl2008 is
end entity;

architecture test of vhdl2008 is
begin

    process is
        variable x, y : integer;
    begin
        x := 1 when y > 2 else 5;       -- OK
        x := 5 when x;                  -- Error
        x := 1 when x < 1 else false;   -- Error
    end process;

end architecture;
