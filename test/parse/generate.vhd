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
    
end architecture;
