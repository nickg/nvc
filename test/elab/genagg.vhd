entity genagg_sub_sub is
    generic (
        DEVA : bit_vector(6 downto 0) );
    port (
        clk   : in bit;
        reset : in bit );
end entity;

architecture rtl of genagg_sub_sub is
begin

end architecture;

-------------------------------------------------------------------------------

entity genagg_sub is
    generic (
        DEVA  : bit_vector(6 downto 0) );
    port (
        clk    : in bit;
        reset  : in bit );
end entity;

architecture rtl of genagg_sub is
begin

    slave_i: entity work.genagg_sub_sub
        generic map (
            DEVA => DEVA )
        port map (
            clk   => clk,
            reset => reset );

end architecture;

-------------------------------------------------------------------------------

entity genagg is
end entity;

architecture test of genagg is
    signal clk    : bit := '0';
    signal reset  : bit := '1';
begin

    uut: entity work.genagg_sub
        generic map (
            DEVA  => "0000101" )
        port map (
            clk   => clk,
            reset => reset );

end architecture;
