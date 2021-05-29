entity basic is
end entity;

architecture test of basic is
    signal x : integer;
begin

    process is
    begin
        x <= 5;
        wait;
    end process;

end architecture;
