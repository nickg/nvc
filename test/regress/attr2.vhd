entity attr2 is
end entity;

architecture test of attr2 is

    type int3d is array (natural range <>,
                         natural range <>,
                         natural range <>) of integer;

begin

    process is
        variable v : int3d(1 to 2, 1 downto 0, 10 to 19);
    begin
        assert v'length(1) = 2;
        assert v'length(2) = 2;
        assert v'length(3) = 10;
        wait;
    end process;

end architecture;
