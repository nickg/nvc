entity comp1_bot is
    port (
        x : in integer;
        y : out integer );
end entity;

architecture rtl of comp1_bot is
begin
    y <= x + 1;
end architecture;

-------------------------------------------------------------------------------

entity comp1 is
end entity;

architecture rtl of comp1 is
    signal a, b : integer;

    component comp1_bot is
        port (
            x : in integer;
            y : out integer );
    end component;
begin

    c1: component comp1_bot
        port map ( 1, a );

    c2: comp1_bot
        port map ( 2, b );

    process is
    begin
        wait for 1 ns;
        assert a = 2;
        assert b = 3;
        wait;
    end process;

end architecture;
