entity attr9 is
end entity;

architecture test of attr9 is
begin

    process is
        type my_small_int is range 1 to 10;
        type my_enum is (A, B, C, D);
        type my_real is range -5.0 to 5.0;
        subtype my_sub is my_enum range B to C;
        type resistance is range 0 to 10000000
            units
                ohm;
                kohm = 1000 ohm;
                mohm = 1000 kohm;
            end units;

        function "="(l, r : my_real) return boolean is
        begin
            return l > r - 0.0001 and l < r + 0.0001;
        end function;

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
        assert my_sub'value(" B  ") = B;

        assert resistance'value("1 ohm") = 1 ohm;
        assert resistance'value("5 ohm") = 5 ohm;
        assert resistance'value("1 kohm") = 1000 ohm;
        assert resistance'value(" 25 kohm   ") = 25_000 ohm;

        assert my_real'value("1.23") = 1.23;
        assert my_real'value(" 4.2  ") = 4.2;

        wait;
    end process;

end architecture;
