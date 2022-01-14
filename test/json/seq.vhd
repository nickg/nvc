entity seq is
end entity;

architecture test of seq is
    signal x : integer;
begin

    process is
    begin
        if x < 5 then
            x <= 5;
        end if;
        wait;
    end process;

end architecture;
