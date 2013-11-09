entity attr9 is
end entity;

architecture test of attr9 is
begin

    process is
        type my_small_int is range 1 to 10;
    begin
        assert integer'value("1") = 1;
        assert natural'value("  12_3") = 123;
        assert my_small_int'value("5  ") = 5;
        assert boolean'value("true") = true;
        assert boolean'value("FALSE") = false;
        assert character'value("'x' ") = 'x';
        wait;
    end process;

end architecture;
