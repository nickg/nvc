entity attr9 is
end entity;

architecture test of attr9 is
begin

    process is
        type my_small_int is range 1 to 10;
        type my_enum is (A, B, C, D);
        subtype my_sub is my_enum range B to C;
    begin
        assert integer'value("1") = 1;
        assert integer'value("-1") = -1;
        assert natural'value("  12_3") = 123;
        assert my_small_int'value("5  ") = 5;
        assert boolean'value("true") = true;
        assert boolean'value("FALSE") = false;
        assert character'value("'x' ") = 'x';

        assert integer'value(integer'image(integer'high)) = integer'high;
        assert integer'value(integer'image(integer'low)) = integer'low;

        assert my_enum'value("A") = A;
        --assert my_sub'value("A") = A;
        wait;
    end process;

end architecture;
