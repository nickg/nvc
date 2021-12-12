entity e is end entity;

architecture a of e is
    signal x, y, z, q, b : bit;
begin

    x <= guarded y;                     -- Error

    with b select z <= guarded q when others;  -- Error

    b1: block (b = '1') is
    begin

        x <= guarded y;                     -- OK

        with b select z <= guarded q when others;  -- OK

        assert guard;                   -- OK

    end block;

    b2: block is
        signal guard : boolean := true;  -- OK
        signal q : bit;
        disconnect q : bit after 0 ns;  -- OK (sem failure)
    begin

        x <= guarded y;                     -- OK

        with b select z <= guarded q when others;  -- OK

    end block;

end architecture;
