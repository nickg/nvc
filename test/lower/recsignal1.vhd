entity recsignal1 is
end entity;

architecture test of recsignal1 is
    type rec is record
        x, y : integer;
    end record;

    signal p, q : rec;
begin

    p1: p <= q;

end architecture;
