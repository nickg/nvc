entity comp4_bot is
    port (
        x : in bit_vector(7 downto 0);
        y : out bit_vector(7 downto 0) );
end entity;

architecture rtl of comp4_bot is
begin
    y <= x;
end architecture;

-------------------------------------------------------------------------------

entity issue17 is
end entity;

architecture rtl of issue17 is
signal b: bit_vector(7 downto 0);

    component comp4_bot is
        port (
            y : out bit_vector(7 downto 0);
            x : in bit_vector(7 downto 0) );
    end component;
begin

    c1: component comp4_bot
        port map ( x=>x"aa", y=>b );

end architecture;
