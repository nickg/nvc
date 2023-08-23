entity comp2 is
end entity;

architecture rtl of comp2 is
    signal a, b : integer;

    component comp2_bot is
        port (
            x : in integer;
            y : out integer := 55 );
    end component;
begin

    c1: component comp2_bot
        port map ( a, b );

    process is
    begin
        wait for 1 ns;
        assert a = integer'left;
        assert b = 55;
        wait;
    end process;

end architecture;
