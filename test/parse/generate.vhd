architecture a of g is
begin

    g1: if foo generate
        signal x : integer;
    begin
        x <= 5;
    end generate;

    g2: if bar generate
        g2a: if x < 5 generate
            g <= 7;
        end generate;
    end generate;

    g3: for i in 1 to 40 generate
        signal x : integer;
    begin
        f <= h;
    end generate;

    g4: for i in x'range generate
    end generate;

    g5: for i in x'range generate
    begin
    end generate;

    g6: for i in 1 to 3 generate
        component sub_ent is
            port (val: out natural);
        end component sub_ent;          -- OK
    begin
    end generate;

    g7: if true generate
        procedure doit is               -- OK
        begin
            write(OUTPUT, "OK." & LF);
        end procedure doit;
    begin
    end generate g7;

end architecture;
