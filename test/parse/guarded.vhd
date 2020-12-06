entity e is end entity;

architecture a of e is
    signal x, y, z, q, b : bit;
begin

    x <= guarded y;

    with b select z <= guarded q when others;

end architecture;
