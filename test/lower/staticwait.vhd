entity staticwait is
end entity;

architecture test of staticwait is
    signal x : integer;
begin

    process (x) is
    begin
        x <= 0;
    end process;

end architecture;
