entity gg is end entity;

architecture aa of gg is
    constant foo, bar : boolean := false;
    signal x, g, f : integer;
    constant h : integer := 6;

    type text is file of string;
    file output : text open WRITE_MODE is "STD_OUTPUT";
begin

    g1: if foo generate
        signal x : integer;
    begin
        x <= 5;
    end generate;

    g2: if bar generate
        g2a: if h < 5 generate
            g <= 7;
        end generate;
    end generate;

    g3: for i in 1 to 40 generate
        signal x : integer;
    begin
        f <= h;
    end generate;

    g4: for i in natural'range generate
    end generate;

    g5: for i in integer'range generate
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
