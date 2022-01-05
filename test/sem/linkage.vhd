entity e is
    port ( x : linkage integer );
end entity;

architecture a of e is
begin

    x <= 1;                             -- Error
    assert x = 1;                       -- Error

    sub: entity work.e port map ( x );  -- OK


end architecture;
