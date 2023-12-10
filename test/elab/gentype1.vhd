entity sub is
    generic ( type t );
end entity;

architecture test of sub is
    signal s : t;
begin
end architecture;

-------------------------------------------------------------------------------

entity gentype1 is
end entity;

architecture test of gentype1 is
begin

    u: entity work.sub
        generic map ( t => bit_vector );  -- Error

end architecture;
