entity test is
end test;

architecture fum of test is
    signal foo : bit_vector(1 downto 0);
    alias foo1 is foo(1);
    alias foo0 is foo(0);
begin
dummy:
    process is

    begin
        for i in foo'range loop   -- range is 1 downto 0
            case i is                   -- OK
                when 0 =>
                    report "foo(0) = " & bit'image(foo0);
                when 1 =>
                    report "foo(1) = " & bit'image(foo1);
            end case;
            case i is                   -- Error
                when 0 =>
                    report "foo(0) = " & bit'image(foo0);
            end case;
        end loop;
        wait;
    end process dummy;
end architecture fum;
