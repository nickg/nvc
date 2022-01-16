entity proc is
end entity;

architecture test of proc is
    signal x, y : integer;
    procedure proc(n : integer) is
    begin
    end procedure;
begin

    -- Test rewrite of process sensitivity list
    process (x, y) is
    begin
        report "awake";
    end process;

    -- Test rewrite of concurrent assignments
    x <= y + 4;
    postponed x <= y + 4 when y < 2 else x + 1 when x < 2 else 0;

    -- Concurrent procedure call to process
    proc(n => 4);

end architecture;
