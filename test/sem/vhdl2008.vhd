entity vhdl2008 is
end entity;

architecture test of vhdl2008 is
begin

    process is
        variable x, y : integer;
    begin
        x := 1 when y > 2 else 5;       -- OK
        x := 5 when x;                  -- Error
        x := 1 when x < 1 else false;   -- Error
    end process;

    -- Changes to locally static rules
    process is
        type r is record
            k : bit;
        end record;
        constant c : bit_vector(1 to 3) := "101";
        constant d : r := ( k => '1' );
        variable x : bit;
        variable y : r;
        variable i : integer;
    begin
        case x is
            when c(1) => null;          -- OK
            when d.k => null;           -- OK
            when c(i) => null;          -- Error
            when others => null;
        end case;
    end process;

    -- 'SUBTYPE attribute
    process is
        variable x : integer;
    begin
        x := baz'subtype(4);            -- Error
        report to_string(x'subtype);    -- Error
    end process;

end architecture;
