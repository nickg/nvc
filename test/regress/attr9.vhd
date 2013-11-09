entity attr9 is
end entity;

architecture test of attr9 is
begin

    process is
    begin
        assert integer'value("1") = 1;
        wait;
    end process;

end architecture;
