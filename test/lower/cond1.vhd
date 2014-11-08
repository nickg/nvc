entity cond1 is
end entity;

architecture test of cond1 is
    signal x : integer := 5;
begin

    process is
        variable y : integer;
    begin
        if x = y then
            y := 2;
        end if;
        if x = y + 1 then
            y := 1;
        else
            y := 3;
            null;
        end if;
        report "done";
        wait;
    end process;

end architecture;
