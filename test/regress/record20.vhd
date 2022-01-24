entity record20 is
end entity;

architecture test of record20 is
    type rec is record
        x : bit_vector(7 downto 0);
    end record;

    signal s : rec;
    signal t : bit_vector(3 downto 0);
begin

    p1: t <= s.x(5 downto 2);

    main: process is
    begin
        s <= ( x => X"ff" );
        wait for 1 ns;
        assert t = X"f";
        s.x(7 downto 4) <= X"0";
        wait for 1 ns;
        assert t = "0011";
        wait;
    end process;

end architecture;
