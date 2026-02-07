entity sub is
    -- This causes an instance name cache to be generated in all further
    -- component blocks
    generic ( gg : string := sub'instance_name );
end entity;

architecture test of sub is
begin
end architecture;

-------------------------------------------------------------------------------

entity clone3 is
    generic ( w : natural := 5 );
end entity;

architecture test of clone3 is
    component comp is
        port ( i : in bit;
               o : out bit_vector(1 to w) );
    end component;

    for all: comp use open;
begin

    u1: component comp port map ('1', open);

    u2: entity work.sub;

    u3: component comp port map ('0', open);

end architecture;
