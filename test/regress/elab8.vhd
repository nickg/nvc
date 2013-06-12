entity sub is
    port (
        foo : out bit_vector(1 to 3) );
end entity;

architecture test of sub is
begin

    process is
    begin
        foo <= "101";
        wait for 10 ns;
        foo <= "010";
        wait for 10 ns;
        foo <= "100";
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity elab8 is
end entity;

architecture test of elab8 is
    signal bar     : bit_vector(1 to 3);
    signal a, b, c : bit;
begin

    sub1_i: entity work.sub
        port map (
            foo(1) => bar(3),
            foo(2) => bar(2),
            foo(3) => bar(1) );

    sub2_i: entity work.sub
        port map (
            foo(1) => a,
            foo(2) => b,
            foo(3) => c );

    process is
    begin
        wait for 1 ns;
        assert a = '1';
        assert b = '0';
        assert c = '1';
        assert bar = "101";
        wait for 10 ns;
        assert a = '0';
        assert b = '1';
        assert c = '0';
        assert bar = "010";
        wait for 10 ns;
        assert a = '1';
        assert b = '0';
        assert c = '0';
        assert bar = "001";
        wait;
    end process;

end architecture;
