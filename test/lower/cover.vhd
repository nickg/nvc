entity cover is
end entity;

architecture test of cover is
    signal s : integer;
begin

    process is
        variable v : integer;
    begin
        v := 1;
        if s = 1 or s > 10 then
            v := 2;
        end if;
        wait;
    end process;

end architecture;
