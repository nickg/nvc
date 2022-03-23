entity sub is
    port (
        o : out bit_vector(1 to 3) );
end entity;

architecture test of sub is

    procedure write1(signal x : out bit) is
    begin
        x <= '1';
    end procedure;

begin

    p1: process is
    begin
        write1(o(1));
        wait for 1 ns;
        write1(o(3));
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity signal19 is
end entity;

architecture test of signal19 is
    signal x : bit_vector(1 to 3);
begin

    uut: entity work.sub port map ( x );

    main: process is
    begin
        wait on x for 100 ns;
        assert now = 0 ns;
        wait on x for 100 ns;
        assert now = 1 ns;
        assert x'active;
        assert not x(1)'active;
        assert x(3)'active;
        assert x = "101";
        wait;
    end process;

end architecture;
