entity ename8 is
end entity;

architecture test of ename8 is
begin

    g: for i in 1 to 1 generate
        signal y : integer := 0;
    begin
        b: block is
            signal x : integer := 0;
        begin
            process is
            begin
                << signal ^.^.g(1).y : integer >> <= 55;
                wait for 1 ns;
                assert x = 42;
                wait;
            end process;
        end block;

        process is
        begin
            << signal b.x : integer >> <= 42;
            wait for 1 ns;
            assert y = 55;
            wait;
        end process;

    end generate;

end architecture;
