entity bounds13 is
end entity;

architecture test of bounds13 is
begin

    process is
        type myint is range 1 to 3;
    begin
        assert myint'value("  3  ") = 3;
        assert myint'value("4") = 1;      -- Error
        wait;
    end process;

end architecture;
