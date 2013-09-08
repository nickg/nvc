entity g is
end entity;

architecture test of g is
    constant c : integer := 5;
    signal x : integer;
    signal b : bit_vector(1 to 5);
begin

    g1: if true generate                -- OK
    begin
        x <= 5;
    end generate;

    g2: if x generate                   -- Error
    begin
    end generate;

    g3: if false generate               -- OK
        signal y : integer;
    begin
        y <= x;
    end generate;

    g4: if false generate
        y <= 5;                         -- Error
    end generate;

    g5: for i in 1 to 5 generate        -- OK
        b(i) <= '1';
    end generate;

    g6: for x in b'range generate       -- OK
        constant k : bit := '1';
    begin
        b(x) <= k;
    end generate;

    g7: for x in b'range generate
        alias a is b(x);                -- OK
    begin
        a <= '1';
    end generate;

    g8: if x > 4 generate               -- Error
    end generate;

    g9: for i in 1 to x generate        -- Error
    end generate;

    g10: for i in integer (-4) to integer (-1) generate  -- OK
    end generate;

end architecture;
