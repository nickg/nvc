entity arith6 is
end entity;

architecture test of arith6 is
begin

    process is
        variable i : integer;
    begin
        assert i < -2 ** 32;            -- Testing 64-bit integers
        wait for 1 ns;
        i := i * 2;                     -- Error
        wait;
    end process;

end architecture;
