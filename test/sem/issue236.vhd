entity subent is
    port (
    a       :   in      boolean := false;
    b       :   in      boolean;
    c       :   inout   string;     -- unconstrained
    d       :   out     string(1 to 4)
    );
end entity;

entity test is
end entity;

architecture a1 of test is
    signal x : string(1 to 3);
begin

    e1: entity work.subent      -- ok
        port map (
        -- a is an unassociated port of mode IN but has a default value
        b   => true,
        c   => x
        -- d is unassociated port of mode OUT but is constrained
    );

    e2: entity work.subent      -- error
        port map (
        a   => true,
        -- b is not allowed to be unassociated
        c   => x
    );

end architecture;

architecture a2 of test is
begin

    e2: entity work.subent      -- error
        port map (
        a   => true,
        b   => true
        -- c is not allowed to be unassociated
    );

end architecture;
