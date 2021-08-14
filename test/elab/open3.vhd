entity sub is
    port (
        x : in bit_vector(1 downto 0);
        y : out bit_vector(1 downto 0) );
end entity;

architecture test of sub is
begin
    y <= x;
end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture test of top is
begin

    sub_i: entity work.sub
        port map (
            x(0) => '1',
            x(1) => '0',
            y => open );

end architecture;
