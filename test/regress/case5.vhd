entity case5 is
end entity;

architecture test of case5 is
begin

    process
        type modcount is range 0 to 7;
        variable a: modcount := 5;
        constant b: natural := 4;
        variable c: natural;
    begin
        case modcount(integer(a) + b) is
            when 0 | 1 | 2 | 3 | 4 =>
                c := c + 1;
            when 5 | 6 | 7 =>
                c := c + 42;
        end case;
        report "integer(a) + b = " & integer'image(natural'VAL(integer(a)+b));
        report "c = " & natural'image(natural'VAL(c));
        wait;
    end process;

end architecture;
