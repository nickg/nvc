entity condvar is
end entity;

architecture test of condvar is
begin

    process is
        variable x, y : integer;
    begin
        x := 1 when y > 2 else 5;       -- OK
        x := 1 when y > 2 else unaffected;  -- OK
    end process;

end architecture;
