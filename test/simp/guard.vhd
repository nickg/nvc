entity guard is
end entity;

architecture test of guard is
    signal value  : natural := 0;
    signal output : natural;
begin

    b1: block (value < 10) is
    begin
        p1: output <= guarded value * 2;
        p2: with output select value <= guarded output + 1 when others;
    end block;

end architecture;
