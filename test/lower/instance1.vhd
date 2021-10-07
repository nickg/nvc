entity sub is
    generic ( WIDTH : integer );
    port ( x : in bit_vector(1 to WIDTH) );
end entity;

architecture test of sub is
begin

end architecture;

-------------------------------------------------------------------------------

entity instance1 is
end entity;

architecture test of instance1 is

    component sub is
        generic ( WIDTH : integer );
        port ( x : in bit_vector(1 to WIDTH) );
    end component;

    constant WIDTH : integer := 5;

begin

    sub_i: sub generic map ( WIDTH ) port map ( "00000" );

end architecture;
