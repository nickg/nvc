entity sub is
    port (
        x : in bit;
        y : out bit_vector(0 downto 0) );
end entity;

architecture test of sub is
begin

    y(0) <= x after 1 ps;

end architecture;

-------------------------------------------------------------------------------

entity elab15 is
end entity;

architecture test of elab15 is
begin

    sub_i: entity work.sub
        port map ( x => '1' );

end architecture;
