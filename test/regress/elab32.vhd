entity sub is
    port ( x : in bit_vector );
begin
    assert x'length = 1;
end entity;

architecture test of sub is
begin

end architecture;

-------------------------------------------------------------------------------

entity elab32 is
end entity;

architecture test of elab32 is
    signal b : bit;
begin

    uut: entity work.sub
        port map ( x(0) => b );

end architecture;
